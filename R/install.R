
#' Install Packages
#'
#' Install one or more \R packages, from a variety of remote sources.
#'
#' `install()` uses the same machinery as [restore()] when installing packages.
#' In particular, this means that the local cache of package installations is
#' used when possible. This helps to avoid re-downloading packages that have
#' already been downloaded before, and re-compiling packages from source when
#' a binary copy of that package is already available.
#'
#'
#' @section Project DESCRIPTION Files:
#'
#' If your project contains a `DESCRIPTION` file, then calling `install()`
#' without any arguments will instruct `renv` to install the latest versions of
#' all packages as declared within that `DESCRIPTION` file's `Depends`,
#' `Imports` and `LinkingTo` fields; similar to how an \R package might declare
#' its dependencies.
#'
#' If you have one or more packages that you'd like to install from a separate
#' remote source, this can be accomplished by adding a `Remotes:` field to the
#' `DESCRIPTION` file. See `vignette("dependencies", package = "devtools")`
#' for more details. Alternatively, view the vignette online at
#' <https://devtools.r-lib.org/articles/dependencies.html>.
#'
#' Note that `install()` does not use the project's `renv.lock` when determining
#' sources for packages to be installed. If you want to install packages using
#' the sources declared in the lockfile, consider using `restore()` instead.
#' Otherwise, you can declare the package sources in your `DESCRIPTION`'s
#' `Remotes:` field.
#'
#'
#' @section Remotes Syntax:
#'
#' `renv` supports a subset of the `remotes` syntax used for package installation,
#' as described in <https://remotes.r-lib.org/articles/dependencies.html>. See
#' the examples below for more details.
#'
#'
#' @section Bioconductor:
#'
#' Packages from Bioconductor can be installed by using the `bioc::` prefix.
#' For example,
#'
#' ```
#' renv::install("bioc::Biobase")
#' ```
#'
#' will install the latest-available version of `Biobase` from Bioconductor.
#'
#' `renv` depends on `BiocManager` (or, for older versions of \R, `BiocInstaller`)
#' for the installation of packages from Bioconductor. If these packages are
#' not available, `renv` will attempt to automatically install them before
#' fulfilling the installation request.
#'
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
#' This could also be specified as, for example,
#'
#' ```
#' options(
#'   configure.args.RNetCDF = "--with-netcdf-include=/usr/include/udunits2"
#' )
#' renv::install("RNetCDF")
#' ```
#'
#' Similarly, additional flags that should be passed to `R CMD INSTALL` can
#' be set via the `install.opts` \R option:
#'
#' ```
#' # installation of R packages using the Windows Subsystem for Linux
#' # may require the `--no-lock` flag to be set during install
#' options(install.opts = "--no-lock")
#' renv::install("xml2")
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
#' # install a package from GitHub, using specific commit
#' renv::install("eddelbuettel/digest@@df55b00bff33e945246eff2586717452e635032f")
#'
#' # install a package from Bioconductor
#' # (note: requires the BiocManager package)
#' renv::install("bioc::Biobase")
#'
#' # install a package, specifying path explicitly
#' renv::install("~/path/to/package")
#'
#' # install packages as declared in the project DESCRIPTION file
#' renv::install()
#'
#' }
install <- function(packages = NULL,
                    ...,
                    library = NULL,
                    type    = NULL,
                    rebuild = FALSE,
                    repos   = NULL,
                    prompt  = interactive(),
                    project = NULL)
{
  renv_consent_check()
  renv_scope_error_handler()

  # allow user to provide additional package names as part of '...'
  dots <- list(...)
  names(dots) <- names(dots) %||% rep.int("", length(dots))
  packages <- c(packages, dots[!nzchar(names(dots))])

  project <- renv_project_resolve(project)
  renv_scope_lock(project = project)

  libpaths <- renv_libpaths_resolve(library)
  renv_scope_libpaths(libpaths)

  type <- type %||% getOption("pkgType")
  renv_scope_options(pkgType = type)

  # override repositories if requested
  repos <- repos %||% config$repos.override()
  if (length(repos))
    renv_scope_options(repos = repos)

  # get and resolve the packages / remotes to be installed
  remotes <- packages %||% renv_project_remotes(project)
  if (empty(remotes)) {
    vwritef("* There are no packages to install.")
    return(invisible(list()))
  }

  # if users have requested the use of pak, delegate there
  if (config$pak.enabled() && !recursing()) {

    renv_pak_init(
      library = library,
      type    = type,
      rebuild = rebuild,
      project = project
    )

    packages <- if (is.list(remotes)) names(remotes) else remotes
    return(renv_pak_install(packages, libpaths))

  }

  # resolve remotes
  remotes <- lapply(remotes, renv_remotes_resolve)
  names(remotes) <- extract_chr(remotes, "Package")

  # update records with requested remotes
  records <- renv_snapshot_r_packages(libpaths = libpaths, project = project)
  records[names(remotes)] <- remotes

  # read remotes from the package DESCRIPTION file and use those to
  # update non-specific package requests
  records <- renv_install_remotes_update(records, project)

  if (!renv_install_preflight(project, libpaths, remotes, prompt)) {
    renv_report_user_cancel()
    return(invisible(list()))
  }

  # ensure package names are resolved if provided
  packages <- if (length(packages)) names(remotes)

  renv_scope_restore(
    project  = project,
    library  = renv_libpaths_default(),
    records  = records,
    packages = packages,
    rebuild  = rebuild
  )

  # retrieve packages
  records <- retrieve(names(remotes))
  if (empty(records)) {
    vwritef("* There are no packages to install.")
    return(invisible(list()))
  }

  # install retrieved records
  renv_install_impl(records)

  # a bit of extra test reporting
  if (renv_tests_running()) {
    fmt <- "Installed %i %s into library at path %s."
    vwritef(
      fmt,
      length(records),
      plural("package", length(records)),
      renv_path_pretty(renv_libpaths_default())
    )
  }

  # check loaded packages and inform user if out-of-sync
  renv_install_postamble(names(records))

  invisible(records)
}

renv_install_impl <- function(records) {

  staged <- renv_config_install_staged()

  if (staged)
    renv_install_staged(records)
  else
    renv_install_default(records)

  invisible(TRUE)

}

renv_install_staged <- function(records) {

  # get current libpaths
  libpaths <- renv_libpaths_all()

  # set up a dummy library path for installation
  templib <- renv_install_staged_library_path()
  on.exit(unlink(templib, recursive = TRUE), add = TRUE)
  renv_scope_libpaths(c(templib, libpaths))

  # perform the install
  renv_install_default(records)

  # migrate packages into true library
  library <- nth(libpaths, 1L)
  sources <- list.files(templib, full.names = TRUE)
  targets <- file.path(library, basename(sources))
  names(targets) <- sources
  enumerate(targets, renv_file_move, overwrite = TRUE)

  # clear filebacked cache entries
  descpaths <- file.path(targets, "DESCRIPTION")
  renv_filebacked_clear("DESCRIPTION", descpaths)
  renv_filebacked_clear("hash", descpaths)

  invisible(targets)

}

renv_install_staged_library_path_impl <- function() {

  # allow user configuration of staged library location

  # retrieve current project, library path
  staging <- Sys.getenv("RENV_PATHS_LIBRARY_STAGING", unset = NA)
  project <- Sys.getenv("RENV_PROJECT", unset = NA)
  libpath <- renv_libpaths_default()

  # determine root directory for staging
  root <- if (!is.na(staging))
    staging
  else if (!is.na(project))
    renv_paths_renv("staging", project = project)
  else
    file.path(libpath, ".renv")

  # attempt to create it
  ok <- catch(ensure_directory(root))
  if (inherits(ok, "error"))
    return(tempfile("renv-staging"))

  # resolve a unique staging directory in this path
  for (i in 1:100) {
    path <- file.path(root, i)
    if (dir.create(path, showWarnings = FALSE))
      return(path)
  }

  # all else fails, use tempfile
  tempfile("renv-staging")

}

# NOTE: on Windows, installing packages into very long paths
# can fail, as R's internal unzip utility does not handle
# long Windows paths well. in addition, an renv project's
# library path tends to be long, exasperating the issue.
# for that reason, we try to use a shorter staging directory
#
# part of the challenge here is that the R temporary directory
# and R library path might reside on different mounts, and so
# we may want to try and avoid installing on one mount and then
# copying to another mount (as that could be slow).
#
# note that using the renv folder might be counter-productive,
# since users will want to use renv in projects sync'ed via
# OneDrive and friends, and we don't want those to lock files
# in the staging directory
renv_install_staged_library_path <- function() {

  # compute path
  path <- renv_install_staged_library_path_impl()

  # create library directory
  ensure_directory(path)

  # try to make sure it has the same permissions as the library itself
  if (!renv_platform_windows()) {
    libpath <- renv_libpaths_default()
    umask <- Sys.umask("0")
    on.exit(Sys.umask(umask), add = TRUE)
    info <- renv_file_info(libpath)
    Sys.chmod(path, info$mode)
  }

  # return the created path
  return(path)

}

renv_install_default <- function(records) {
  state <- renv_restore_state()
  handler <- state$handler
  for (record in records) {
    package <- record$Package
    handler(package, renv_install_package(record))
  }
}

renv_install_package <- function(record) {

  # get active project (if any)
  state <- renv_restore_state()
  project <- state$project

  # figure out whether we can use the cache during install
  # use library path recorded in restore state as staged installs will have
  # mutated the library path, placing a staging library at the front
  library <- renv_restore_state("library")
  linkable <- renv_cache_linkable(project = project, library = library)
  linker <- if (linkable) renv_file_link else renv_file_copy

  cacheable <-
    renv_cache_config_enabled(project = project) &&
    renv_record_cacheable(record) &&
    !renv_restore_rebuild_required(record)

  if (cacheable) {

    # check for cache entry and install if there
    path <- renv_cache_find(record)
    if (renv_cache_package_validate(path))
      return(renv_install_package_cache(record, path, linker))

  }

  # install the package
  withCallingHandlers(
    renv_install_package_impl(record),
    error = function(e) {
      vwritef("\tFAILED")
      writef(e$output)
    }
  )

  path <- record$Path
  type <- renv_package_type(path, quiet = TRUE)

  feedback <- if (type == "binary") {
    if (renv_file_type(path, symlinks = FALSE) == "directory") {
      "copied local binary"
    } else {
      "installed binary"
    }
  } else {
    "built from source"
  }

  vwritef("\tOK [%s]", feedback)

  # link into cache
  if (renv_cache_config_enabled(project = project))
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

  vwritef("\tOK [%s cache]", type)

  return(TRUE)

}

renv_install_package_cache_skip <- function(record, cache) {

  # don't skip if installation was explicitly requested
  if (record$Package %in% renv_restore_state("packages"))
    return(FALSE)

  # check for matching cache + target paths
  library <- renv_restore_state("library") %||% renv_libpaths_default()
  target <- file.path(library, record$Package)

  renv_file_same(cache, target)

}

renv_install_package_preamble <- function(record) {
  fmt <- "Installing %s [%s] ..."
  with(record, vwritef(fmt, Package, Version))
}

renv_install_package_impl_prebuild <- function(record, quiet) {

  # if this package already appears to be built, nothing to do
  path <- record$Path
  if (renv_package_built(path))
    return(path)

  # check whether user wants us to build before install
  if (!identical(config$install.build(), TRUE))
    return(path)

  # if this is an archive, we'll need to unpack it first
  info <- renv_file_info(path)
  if (identical(info$isdir, FALSE)) {

    # find the package directory
    files <- renv_archive_list(path)
    descpath <- grep("(?:^|/)DESCRIPTION$", files, value = TRUE)
    pkgpath <- dirname(descpath)[nchar(descpath) == min(nchar(descpath))]

    # extract to temporary directory
    exdir <- tempfile("renv-build-")
    ensure_directory(exdir)
    renv_archive_decompress(path, exdir = exdir)

    # update path to package
    path <- file.path(exdir, pkgpath)

    # and ensure we build in this directory
    owd <- setwd(path)
    on.exit(setwd(owd), add = TRUE)

  }

  # if this package depends on a VignetteBuilder that is not installed,
  # then we can't proceed
  descpath <- file.path(path, "DESCRIPTION")
  desc <- renv_description_read(descpath)
  builder <- desc[["VignetteBuilder"]]
  if (!is.null(builder) && !renv_package_installed(builder)) {
    fmt <- "Skipping package build: vignette builder '%s' is not installed"
    vwritef(fmt, builder)
    return(record$Path)
  }

  fmt <- "Building %s [%s] ..."
  with(record, vwritef(fmt, Package, Version))

  before <- Sys.time()
  package <- record$Package
  newpath <- r_cmd_build(package, path)
  after <- Sys.time()
  time <- difftime(after, before, units = "auto")

  fmt <- "\tOK [built package in %s]"
  vwritef(fmt, renv_difftime_format(time))

  newpath

}

renv_install_package_unpack <- function(package, path, force = FALSE) {

  # if this isn't an archive, nothing to do
  info <- renv_file_info(path)
  if (identical(info$isdir, TRUE))
    return(path)

  # list files in the archive
  files <- renv_archive_list(path)

  # if we have a top-level DESCRIPTION file, nothing to
  descpaths <- renv_archive_find(path, "(?:^|/)DESCRIPTION$")
  n <- nchar(descpaths)
  descpath <- descpaths[n == min(n)]

  # if we already have a top-level DESCRIPTION file, nothing to do
  if (!force && dirname(descpath) == package)
    return(path)

  # create extraction directory
  old <- tempfile("renv-package-old-")
  new <- tempfile("renv-package-new-")
  ensure_directory(c(old, new))

  # decompress archive to dir
  renv_archive_decompress(path, exdir = old)

  # rename (without sub-directory)
  oldpath <- file.path(old, dirname(descpath))
  newpath <- file.path(new, package)
  file.rename(oldpath, newpath)

  # use newpath
  newpath

}

renv_install_package_impl <- function(record, quiet = TRUE) {

  package <- record$Package

  # get user-defined options to apply during installation
  options <- renv_install_package_options(package)

  # get archive path for package
  path <- record$Path

  # check whether we should build before install
  path <- renv_install_package_impl_prebuild(record, quiet)

  # report that we're about to start installation
  renv_install_package_preamble(record)

  # for directories, we may need to use subdir to find the package path
  info <- renv_file_info(path)
  subdir <- record$RemoteSubdir %||% ""
  if (identical(info$isdir, TRUE) && nzchar(subdir)) {
    components <- c(path, subdir)
    path <- paste(components, collapse = "/")
  }

  # re-pack package archives if they appear to have their package
  # sources contained as part of a sub-directory
  # TODO: we should probably do this earlier?
  path <- renv_install_package_unpack(package, path)

  # run user-defined hooks before, after install
  before <- options$before.install %||% identity
  after  <- options$after.install %||% identity

  before(package)
  on.exit(after(package), add = TRUE)

  # backup an existing installation of the package if it exists
  library <- renv_libpaths_default()
  destination <- file.path(library, package)
  callback <- renv_file_backup(destination)
  on.exit(callback(), add = TRUE)

  # normalize paths
  path <- renv_path_normalize(path, winslash = "/", mustWork = TRUE)

  # get library path
  library <- renv_libpaths_default()

  # if a package already exists at that path, back it up first
  # this avoids problems with older versions of R attempting to
  # overwrite a pre-existing symlink
  #
  # https://github.com/rstudio/renv/issues/611
  installpath <- file.path(library, package)
  callback <- renv_file_backup(installpath)
  on.exit(callback(), add = TRUE)

  # if this failed for some reason, just remove it
  if (renv_file_broken(installpath))
    renv_file_remove(installpath)

  # if this is the path to an unpacked binary archive,
  # we can just copy the folder over
  copyable <-
    renv_file_type(path, symlinks = FALSE) == "directory" &&
    renv_package_type(path, quiet = TRUE) == "binary"

  # shortcut via copying a binary directory if possible,
  # otherwise, install the package
  if (copyable)
    renv_file_copy(path, installpath, overwrite = TRUE)
  else
    r_cmd_install(package, path)

  # augment package metadata after install
  renv_package_augment(installpath, record)

  # return the path to the package
  invisible(installpath)

}

renv_install_package_options <- function(package) {
  options <- getOption("renv.install.package.options")
  options[[package]]
}

# nocov start
renv_install_preflight_requirements <- function(records) {

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

  bad <- bind(unname(bad))
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
      "The following issues were discovered while preparing for installation:",
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

  if (renv_tests_running()) {
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
  renv_check_unknown_source(records)
}

renv_install_preflight_permissions <- function(library) {

  # try creating and deleting a directory in the library folder
  file <- tempfile(".renv-write-test-", tmpdir = library)
  dir.create(file, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(file, recursive = TRUE), add = TRUE)

  # check if we created the directory successfully
  info <- renv_file_info(file)
  if (identical(info$isdir, TRUE))
    return(TRUE)

  # nocov start
  if (renv_verbose()) {

    # construct header for message
    preamble <- "renv appears to be unable to access the requested library path:"

    # construct footer for message
    info <- as.list(Sys.info())
    fmt <- "Check that the '%s' user has read / write access to this directory."
    postamble <- sprintf(fmt, info$effective_user %||% info$user)

    # print it
    renv_pretty_print(
      values = library,
      preamble = preamble,
      postamble = postamble,
      wrap = FALSE
    )

  }
  # nocov end

  FALSE

}

renv_install_preflight <- function(project, libpaths, records, prompt) {

  # check for packages installed from an unknown source
  library <- nth(libpaths, 1L)

  ok <- all(
    renv_install_preflight_unknown_source(records),
    renv_install_preflight_permissions(library)
  )

  if (ok)
    return(TRUE)

  if (prompt && !proceed())
    return(FALSE)

  TRUE

}

renv_install_remotes_update <- function(records, project) {

  # check for DESCRIPTION file
  descpath <- file.path(project, "DESCRIPTION")
  if (!file.exists(descpath))
    return(records)

  # read Remotes field (if any)
  remotes <- renv_description_remotes(descpath)
  if (empty(remotes))
    return(records)

  # update records as appropriate
  enumerate(remotes, function(package, remote) {

    record <- records[[package]]

    update <-
      is.null(record) ||
      identical(record, list(Package = package, Source = "Repository"))

    if (update)
      records[[package]] <<- remote

  })

  # return updated set of records
  records

}
