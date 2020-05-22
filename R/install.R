
#' Install Packages
#'
#' Install one or more \R packages from a variety of remote sources.
#'
#' `install()` uses the same machinery as [restore()] when installing packages.
#' In particular, this means that the local cache of package installations is
#' used when possible. This helps to avoid re-downloading packages that have
#' already been downloaded before, and re-compiling packages from source when
#' a binary copy of that package is already available.
#'
#' Note that this interface is subject to change -- the goal is to hook into
#' separate package installation backends in the future.
#'
#' @section Package Configuration:
#'
#' Many \R packages have a `configure` script that needs to be run to prepare
#' the package for installation. Arguments and environment variables can be
#' passed through to those scripts in a manner similar to [install.packages].
#' In particular, the \R options `configure.args` and `configure.vars` can be
#' used to map package names to their appropriate configuration. For example:
#'
#' ```
#' # installation of RNetCDF may require us to set include paths for netcdf
#' configure.args = c(RNetCDF = "--with-netcdf-include=/usr/include/udunits2"))
#' options(configure.args = configure.args)
#' renv::install("RNetCDF")
#' ```
#'
#' @inherit renv-params
#' @inheritParams install-params
#'
#' @return A named list of package records which were installed by `renv`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # install the latest version of 'digest'
#' renv::install("digest")
#'
#' # install an old version of 'digest' (using archives)
#' renv::install("digest@@0.6.18")
#'
#' # install 'digest' from GitHub (latest dev. version)
#' renv::install("eddelbuettel/digest")
#'
#' # install a package from local sources
#' renv::install("~/path/to/package")
#'
#' }
install <- function(packages = NULL,
                    ...,
                    library = NULL,
                    type    = NULL,
                    rebuild = FALSE,
                    prompt  = interactive(),
                    project = NULL)
{
  renv_consent_check()
  renv_scope_error_handler()

  dots <- list(...)
  names(dots) <- names(dots) %||% rep.int("", length(dots))
  packages <- c(packages, dots[!nzchar(names(dots))])

  project <- renv_project_resolve(project)
  library <- library %||% renv_libpaths_all()

  type <- type %||% getOption("pkgType")
  renv_scope_options(pkgType = type)

  packages <- packages %||% renv_project_records(project)
  if (is.null(packages))
    stopf("no packages specified in renv::install() request")

  if (empty(packages)) {
    vwritef("* There are no packages to install.")
    return(invisible(list()))
  }

  # override repositories if requested
  repos <- config$repos.override()
  if (length(repos))
    renv_scope_options(repos = repos)

  records <- renv_snapshot_r_packages(library = library, project = project)
  remotes <- lapply(packages, renv_remotes_resolve)

  packages <- extract_chr(remotes, "Package")
  names(remotes) <- packages
  records[names(remotes)] <- remotes

  if (!renv_install_preflight(project, library, remotes, prompt)) {
    message("* Operation aborted.")
    return(invisible(list()))
  }

  rebuild <- case(
    identical(rebuild, TRUE)  ~ packages,
    identical(rebuild, FALSE) ~ character(),
    identical(rebuild, "*")   ~ NA_character_,
    as.character(rebuild)
  )

  renv_scope_restore(
    project = project,
    records = records,
    packages = packages,
    rebuild = rebuild
  )

  # retrieve packages
  records <- renv_retrieve(packages)
  renv_install(records, library)

  # perform auto snapshot
  if (library[[1]] == renv_paths_library(project = project))
    renv_snapshot_auto(project = project)

  # check loaded packages and inform user if out-of-sync
  renv_install_postamble(names(records))

  invisible(records)
}

renv_install <- function(records, library) {

  staged <-
    config$install.transactional(default = NULL) %||%
    config$install.staged()

  if (staged)
    renv_install_staged(records, library)
  else
    renv_install_default(records, library)

  invisible(TRUE)

}

renv_install_staged <- function(records, library) {

  # save active library
  renv_global_set("library.paths", library)
  on.exit(renv_global_clear("library.paths"), add = TRUE)

  # set up a dummy library path for installation
  templib <- renv_install_staged_library(library)
  on.exit(unlink(templib, recursive = TRUE), add = TRUE)
  renv_scope_libpaths(c(templib, renv_libpaths_all()))

  # perform the install
  renv_install_default(records, library)

  # migrate packages into true library
  sources <- list.files(templib, full.names = TRUE)
  targets <- file.path(library[[1]], basename(sources))
  names(targets) <- sources
  enumerate(targets, renv_file_move, overwrite = TRUE)

  # clear filebacked cache entries
  descpaths <- file.path(targets, "DESCRIPTION")
  renv_filebacked_clear("DESCRIPTION", descpaths)
  renv_filebacked_clear("hash", descpaths)

  invisible(targets)

}

renv_install_staged_library_impl <- function(root) {

  ensure_directory(root)
  for (i in 1:100) {
    path <- file.path(root, i)
    if (dir.create(path, showWarnings = FALSE))
      return(path)
  }

  tempfile(".renv-staging")

}

# NOTE: on Windows, installing packages into very long paths
# can fail, as R's internal unzip utility does not handle
# long Windows paths well. in addition, an renv project's
# library path tends to be long, exaspeating the issue.
# for that reason, we try to use a shorter staging directory
# when possible
renv_install_staged_library <- function(library) {

  project <- renv_project(default = NULL)

  root <- if (is.null(project))
    file.path(library[[1]], ".renv")
  else
    file.path(project, "renv/staging")

  renv_install_staged_library_impl(root)

}

renv_install_default <- function(records, library) {
  state <- renv_restore_state()
  handler <- state$handler
  for (record in records) {
    package <- record$Package
    handler(package, renv_install_impl(record))
  }
}

renv_install_impl <- function(record) {

  # get active project (if any)
  state <- renv_restore_state()
  project <- state$project

  # figure out whether we can use the cache during install
  libpaths <- renv_global_get("library.paths") %||% renv_libpaths_all()
  library <- libpaths[[1]]
  linkable <-
    settings$use.cache(project = project) &&
    renv_path_same(library, renv_paths_library(project = project))

  linker <- if (linkable) renv_file_link else renv_file_copy

  cacheable <-
    settings$use.cache(project = project) &&
    renv_record_cacheable(record) &&
    !renv_restore_rebuild_required(record)

  if (cacheable) {

    # check for cache entry and install if there
    path <- renv_cache_find(record)
    if (renv_cache_package_validate(path))
      return(renv_install_package_cache(record, path, linker))

  }

  # report that we're about to start installation
  renv_install_package_preamble(record)

  withCallingHandlers(
    renv_install_package_local(record),
    error = function(e) {
      vwritef("\tFAILED")
      writef(e$output)
    }
  )

  type <- renv_package_type(record$Path, quiet = TRUE)
  feedback <- if (type == "binary")
    "installed binary"
  else
    "built from source"

  vwritef("\tOK [%s]", feedback)

  # link into cache
  if (settings$use.cache(project = project))
    renv_cache_synchronize(record, linkable = linkable)

}

renv_install_package_cache <- function(record, cache, linker) {

  if (renv_install_package_cache_skip(record, cache))
    return(TRUE)

  library <- renv_libpaths_default()
  target <- file.path(library, record$Package)

  # back up the previous installation if needed
  callback <- renv_file_backup(target)
  on.exit(callback(), add = TRUE)

  # report successful link to user
  fmt <- "Installing %s [%s] ..."
  with(record, vwritef(fmt, Package, Version))
  linker(cache, target)

  type <- case(
    identical(linker, renv_file_copy) ~ "copied",
    identical(linker, renv_file_link) ~ "linked"
  )

  vwritef("\tOK (%s cache)", type)

  return(TRUE)

}

renv_install_package_cache_skip <- function(record, cache) {

  state <- renv_restore_state()

  # don't skip if installation was explicitly requested
  if (record$Package %in% state$packages)
    return(FALSE)

  # check for matching cache + target paths
  libpaths <- renv_global_get("library.paths") %||% renv_libpaths_all()
  library <- libpaths[[1]]
  target <- file.path(library, record$Package)

  renv_file_same(cache, target)

}

renv_install_package_preamble <- function(record) {
  fmt <- "Installing %s [%s] ..."
  with(record, vwritef(fmt, Package, Version))
}

renv_install_package_local <- function(record, quiet = TRUE) {

  package <- record$Package

  # get user-defined options to apply during installation
  options <- renv_install_package_options(package)

  # get archive path for package
  library <- renv_libpaths_default()
  path <- record$Path

  # for packages living within a sub-directory, we need to
  # unpack the archive explicitly and update the path
  subdir <- record$RemoteSubdir %||% ""

  # for source packages downloaded as zips,
  # we need to extract before install
  unpack <-
    renv_archive_type(path) == "zip" &&
    renv_package_type(path) == "source" ||
    nzchar(subdir)

  if (unpack) {

    # create extraction directory
    dir <- tempfile("renv-package-")
    ensure_directory(dir)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)

    # decompress archive to dir
    renv_archive_decompress(path, exdir = dir)

    # rename to true package name
    name <- list.files(dir)
    renv_file_move(file.path(dir, name), file.path(dir, package))

    # form new path
    components <- c(dir, package, if (nzchar(subdir)) subdir)
    path <- paste(components, collapse = "/")

  }

  # run user-defined hooks before, after install
  before <- options$before.install %||% identity
  after  <- options$after.install %||% identity

  before(package)
  on.exit(after(package), add = TRUE)

  destination <- file.path(library, package)
  callback <- renv_file_backup(destination)
  on.exit(callback(), add = TRUE)

  # install the package
  renv_install_package_local_impl(package, path, library)

  # augment package metadata after install
  installpath <- file.path(library, package)
  renv_package_augment(installpath, record)

  # return the path to the package
  invisible(installpath)

}

renv_install_package_local_impl <- function(package, path, library) {
  library <- renv_path_normalize(library, winslash = "/", mustWork = TRUE)
  path <- renv_path_normalize(path, winslash = "/", mustWork = TRUE)
  r_cmd_install(package, path, library)
}

renv_install_package_options <- function(package) {
  options <- getOption("renv.install.package.options")
  options[[package]]
}

# nocov start
renv_install_preflight <- function(records) {

  deps <- bapply(records, function(record) {
    renv_dependencies_discover_description(record$Path)
  }, index = "ParentPackage")

  splat <- split(deps, deps$Package)
  bad <- enumerate(splat, function(package, requirements) {

    # skip NULL records (should be handled above)
    record <- records[[package]]
    if (is.null(record))
      return(NULL)

    version <- record$Version

    # drop packages without explicit version requirement
    requirements <- requirements[nzchar(requirements$Require), ]
    if (nrow(requirements) == 0)
      return(NULL)

    # add in requested version
    requirements$RequestedVersion <- version

    # generate expressions to evaluate
    fmt <- "package_version('%s') %s package_version('%s')"
    code <- with(requirements, sprintf(fmt, RequestedVersion, Require, Version))
    parsed <- parse(text = code)
    ok <- map_lgl(parsed, eval, envir = baseenv())

    # return requirements that weren't satisfied
    requirements[!ok, ]

  })

  bad <- bind_list(unname(bad))
  if (empty(bad))
    return(TRUE)

  package  <- bad$ParentPackage
  requires <- sprintf("%s (%s %s)", bad$Package, bad$Require, bad$Version)
  actual   <- sprintf("%s %s", bad$Package, bad$RequestedVersion)

  fmt <- "Package '%s' requires '%s', but '%s' will be installed"
  text <- sprintf(fmt, format(package), format(requires), format(actual))
  if (renv_verbose()) {
    renv_pretty_print(
      text,
      "The following issues were discovered during installation:",
      "Installation of these packages may not succeed.",
      wrap = FALSE
    )
  }

  if (interactive() && !proceed())
    return(FALSE)

  TRUE

}
# nocov end

renv_install_postamble <- function(packages) {

  # only diagnose packages currently loaded
  packages <- renv_vector_intersect(packages, loadedNamespaces())
  if (empty(packages))
    return(TRUE)

  # get version of package in library
  installed <- map_chr(packages, renv_package_version)

  # get version of package currently loaded
  loaded <- map_chr(packages, renv_namespace_version)

  # collect into data.frame
  data <- data.frame(
    Package   = packages,
    Installed = installed,
    Loaded    = loaded,
    stringsAsFactors = FALSE
  )

  # only keep mismatches
  mismatches <- data[data$Installed != data$Loaded, ]
  if (nrow(mismatches) == 0)
    return(TRUE)

  # format and print
  text <- with(mismatches, {
    fmt <- "%s [installed version %s != loaded version %s]"
    sprintf(fmt, format(Package), format(Installed), format(Loaded))
  })

  if (renv_testing()) {
    condition <- "renv.install.restart_required"
    renv_condition_signal(condition)
  }

  # nocov start
  if (renv_verbose()) {
    renv_pretty_print(
      text,
      "The following package(s) have been updated:",
      "Consider restarting the R session and loading the newly-installed packages.",
      wrap = FALSE
    )
  }
  # nocov end

  TRUE

}

renv_install_preflight_unknown_source <- function(records) {

  unknown <- filter(records, function(record) {
    renv_record_source(record) == "unknown"
  })

  if (empty(unknown))
    return(TRUE)

  # nocov start
  if (renv_verbose()) {
    renv_pretty_print_records(
      unknown,
      "The following package(s) were installed from an unknown source:",
      "renv may be unable to restore these packages."
    )
  }
  # nocov end

  FALSE

}

renv_install_preflight_permissions <- function(library) {

  # check for inability to install in requested library
  access <- file.access(library, 7)
  if (access == 0L)
    return(TRUE)

  # nocov start
  if (renv_verbose()) {
    renv_pretty_print(
      library,
      "You do not have permissions to read / write into the requested library:",
      "renv may be unable to restore packages."
    )
  }
  # nocov end

  FALSE

}

renv_install_preflight <- function(project, library, records, prompt) {

  # check for packages installed from an unknown source
  ok <- all(
    renv_install_preflight_unknown_source(records),
    renv_install_preflight_permissions(library[[1]])
  )

  if (ok)
    return(TRUE)

  if (prompt && !proceed())
    return(FALSE)

  TRUE

}
