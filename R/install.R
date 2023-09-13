
# an explicitly-requested package type in a call to 'install()'
the$install_pkg_type <- NULL

# an explicitly-requested dependencies field in a call to 'install()'
the$install_dependency_fields <- NULL

# the formatted width of installation steps printed to the console
the$install_step_width <- 48L

#' Install packages
#'
#' @description
#' Install one or more \R packages, from a variety of remote sources.
#' `install()` uses the same machinery as [restore()] (i.e. it uses cached
#' packages where possible) but it does not respect the lockfile, instead
#' installing the latest versions available from CRAN.
#'
#' See `vignette("package-install")` for more details.
#'
#' # `Remotes`
#'
#' `install()` (called without arguments) will respect the `Remotes` field
#' of the `DESCRIPTION` file (if present). This allows you to specify places
#' to install a package other than the latest version from CRAN.
#' See <https://remotes.r-lib.org/articles/dependencies.html> for details.
#'
#' # Bioconductor
#'
#' Packages from Bioconductor can be installed by using the `bioc::` prefix.
#' For example,
#'
#' ```
#' renv::install("bioc::Biobase")
#' ```
#'
#' will install the latest-available version of Biobase from Bioconductor.
#'
#' renv depends on BiocManager (or, for older versions of \R, BiocInstaller)
#' for the installation of packages from Bioconductor. If these packages are
#' not available, renv will attempt to automatically install them before
#' fulfilling the installation request.
#'
#' @inherit renv-params
#'
#' @param packages Either `NULL` (the default) to install all packages required
#'  by the project, or a character vector of packages to install. renv
#'  supports a subset of the remotes syntax used for package installation,
#'  e.g:
#'
#'  * `pkg`: install latest version of `pkg` from CRAN.
#'  * `pkg@version`: install specified version of `pkg` from CRAN.
#'  * `username/repo`: install package from GitHub
#'  * `bioc::pkg`: install `pkg` from Bioconductor.
#'
#'  See <https://remotes.r-lib.org/articles/dependencies.html> and the examples
#'  below for more details.
#'
#'  renv deviates from the remotes spec in one important way: subdirectories
#'  are separated from the main repository specification with a `:`, not `/`.
#'  So to install from the `subdir` subdirectory of GitHub package
#'  `username/repo` you'd use `"username/repo:subdir`.
#'
#' @param exclude Packages which should not be installed. `exclude` is useful
#'   when using `renv::install()` to install all dependencies in a project,
#'   except for a specific set of packages.
#'
#' @return A named list of package records which were installed by renv.
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
                    exclude      = NULL,
                    library      = NULL,
                    type         = NULL,
                    rebuild      = FALSE,
                    repos        = NULL,
                    prompt       = interactive(),
                    dependencies = NULL,
                    project      = NULL)
{
  renv_consent_check()
  renv_scope_error_handler()

  # allow user to provide additional package names as part of '...'
  if (!missing(...)) {
    dots <- list(...)
    names(dots) <- names(dots) %||% rep.int("", length(dots))
    packages <- c(packages, dots[!nzchar(names(dots))])
  }

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)
  renv_scope_verbose_if(prompt)

  # handle 'dependencies'
  if (!is.null(dependencies)) {
    fields <- renv_description_dependency_fields(dependencies, project = project)
    renv_scope_binding(the, "install_dependency_fields", fields)
  }

  # set up library paths
  libpaths <- renv_libpaths_resolve(library)
  renv_scope_libpaths(libpaths)

  # check for explicitly-provided type -- we handle this specially for PPM
  if (!is.null(type)) {
    renv_scope_binding(the, "install_pkg_type", type)
    renv_scope_options(pkgType = type)
  }

  # override repositories if requested
  repos <- repos %||% config$repos.override()
  if (length(repos))
    renv_scope_options(repos = repos)

  # if users have requested the use of pak, delegate there
  if (config$pak.enabled() && !recursing()) {
    renv_pak_init()
    return(renv_pak_install(packages, libpaths, project))
  }

  # resolve remotes from explicitly-requested packages
  remotes <- if (length(packages)) {
    remotes <- map(packages, renv_remotes_resolve)
    names(remotes) <- map_chr(remotes, `[[`, "Package")
    remotes
  }

  # figure out which packages we should install
  packages <- names(remotes) %||% renv_snapshot_dependencies(project, dev = TRUE)

  # apply exclude parameter
  if (length(exclude))
    packages <- setdiff(packages, exclude)

  if (empty(packages)) {
    writef("- There are no packages to install.")
    return(invisible(list()))
  }

  # add bioconductor packages if necessary
  if (renv_bioconductor_required(remotes)) {
    bioc <- c(renv_bioconductor_manager(), "BiocVersion")
    packages <- unique(c(packages, bioc))
  }

  # don't update renv unless it was explicitly requested
  if (!"renv" %in% names(remotes))
    packages <- setdiff(packages, "renv")

  # start building a list of records; they should be resolved this priority:
  #
  # 1. explicit requests from the user
  # 2. remotes declarations from the DESCRIPTION file
  # 3. existing version in library, if any
  # 4. fallback to package repositories
  #
  # we overlay 1 and 2 here, and then do 3 and 4 dynamically if required
  # during the retrieve + install stages
  records <- overlay(renv_project_remotes(project), remotes)

  # run install preflight checks
  if (!renv_install_preflight(project, libpaths, records))
    cancel_if(prompt && !proceed())

  # we're now ready to start installation
  renv_scope_restore(
    project  = project,
    library  = renv_libpaths_active(),
    packages = names(remotes),
    records  = records,
    rebuild  = rebuild
  )

  # retrieve packages
  records <- retrieve(packages)
  if (empty(records)) {
    writef("- There are no packages to install.")
    return(invisible(list()))
  }

  if (prompt || renv_verbose()) {
    renv_install_report(records, library = renv_libpaths_active())
    cancel_if(prompt && !proceed())
  }

  # install retrieved records
  before <- Sys.time()
  renv_install_impl(records)
  after <- Sys.time()

  time <- renv_difftime_format(difftime(after, before))
  n <- length(records)
  writef("Successfully installed %s in %s.", nplural("package", n), time)

  # check loaded packages and inform user if out-of-sync
  renv_install_postamble(names(records))

  invisible(records)
}

renv_install_impl <- function(records) {

  staged <- renv_config_install_staged()

  writef(header("Installing packages"))

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
  defer(unlink(templib, recursive = TRUE))
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
  renv_filebacked_clear("renv_description_read", descpaths)
  renv_filebacked_clear("renv_hash_description", descpaths)

  invisible(targets)

}

renv_install_staged_library_path_impl <- function() {

  # get current library path
  libpath <- renv_libpaths_active()

  # retrieve current project, library path
  stagedlib <- local({

    # allow user configuration of staged library location
    override <- Sys.getenv("RENV_PATHS_LIBRARY_STAGING", unset = NA)
    if (!is.na(override))
      return(override)

    # if we have an active project, use that path
    project <- renv_project_get(default = NULL)
    if (!is.null(project))
      return(renv_paths_renv("staging", project = project))

    # otherwise, stage within library path
    file.path(libpath, ".renv")

  })

  # attempt to create it
  ok <- catch(ensure_directory(stagedlib))
  if (inherits(ok, "error"))
    return(tempfile("renv-staging-"))

  # resolve a unique staging directory in this path
  # we want to keep paths short just in case; it's easy to blow up the
  # path length limit (hence we don't use tempfile below)
  for (i in 1:100) {
    path <- file.path(stagedlib, i)
    if (dir.create(path, showWarnings = FALSE))
      return(path)
  }

  # all else fails, use tempfile
  tempfile("renv-staging-")

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
    libpath <- renv_libpaths_active()
    umask <- Sys.umask("0")
    defer(Sys.umask(umask))
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
  before <- Sys.time()
  withCallingHandlers(
    renv_install_package_impl(record),
    error = function(e) writef("FAILED")
  )
  after <- Sys.time()

  path <- record$Path
  type <- renv_package_type(path, quiet = TRUE)
  feedback <- renv_install_package_feedback(path, type)


  # link into cache
  if (renv_cache_config_enabled(project = project)) {
    renv_cache_synchronize(record, linkable = linkable)
    feedback <- paste0(feedback, " and cached")
  }

  elapsed <- difftime(after, before, units = "auto")
  renv_install_step_ok(feedback, elapsed = elapsed)

  invisible()

}

renv_install_package_feedback <- function(path, type) {

  if (identical(type, "source"))
    return("built from source")

  if (renv_file_type(path, symlinks = FALSE) == "directory")
    return("copied local binary")

  "installed binary"

}

renv_install_package_cache <- function(record, cache, linker) {

  if (renv_install_package_cache_skip(record, cache))
    return(TRUE)

  library <- renv_libpaths_active()
  target <- file.path(library, record$Package)

  # back up the previous installation if needed
  callback <- renv_file_backup(target)
  defer(callback())

  # report successful link to user
  renv_install_step_start("Installing", record$Package)

  before <- Sys.time()
  linker(cache, target)
  after <- Sys.time()

  type <- case(
    identical(linker, renv_file_copy) ~ "copied from cache",
    identical(linker, renv_file_link) ~ "linked from cache"
  )

  elapsed <- difftime(after, before, units = "auto")
  renv_install_step_ok(type, elapsed = elapsed)

  return(TRUE)

}

renv_install_package_cache_skip <- function(record, cache) {

  # don't skip if installation was explicitly requested
  if (record$Package %in% renv_restore_state("packages"))
    return(FALSE)

  # check for matching cache + target paths
  library <- renv_restore_state("library") %||% renv_libpaths_active()
  target <- file.path(library, record$Package)

  renv_file_same(cache, target)

}

renv_install_package_impl_prebuild <- function(record, path, quiet) {

  # check whether user wants us to build before install
  if (!identical(config$install.build(), TRUE))
    return(path)

  # if this package already appears to be built, nothing to do
  if (renv_package_built(path))
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
    renv_scope_wd(path)

  }

  # if this package depends on a VignetteBuilder that is not installed,
  # then we can't proceed
  descpath <- file.path(path, "DESCRIPTION")
  desc <- renv_description_read(descpath)
  builder <- desc[["VignetteBuilder"]]
  if (!is.null(builder) && !renv_package_installed(builder)) {
    fmt <- "Skipping package build: vignette builder '%s' is not installed"
    writef(fmt, builder)
    return(path)
  }

  renv_install_step_start("Building", record$Package)

  before <- Sys.time()
  package <- record$Package
  newpath <- r_cmd_build(package, path)
  after <- Sys.time()
  elapsed <- difftime(after, before, units = "auto")

  renv_install_step_ok("from source", elapsed = elapsed)

  newpath

}

renv_install_package_impl <- function(record, quiet = TRUE) {

  package <- record$Package

  # get path for package
  path <- record$Path

  # check if it's an archive (versus an unpacked directory)
  info <- renv_file_info(path)
  isarchive <- identical(info$isdir, FALSE)

  subdir <- record$RemoteSubdir %||% ""
  if (isarchive) {
    # re-pack archives if they appear to have their package
    # sources contained as part of a sub-directory
    path <- renv_package_unpack(package, path, subdir = subdir)
  } else if (nzchar(subdir)) {
    # for directories, we may need to use subdir to find the package path
    components <- c(path, subdir)
    path <- paste(components, collapse = "/")
  }

  # check whether we should build before install
  path <- renv_install_package_impl_prebuild(record, path, quiet)
  renv_install_step_start("Installing", record$Package)

  # run user-defined hooks before, after install
  options <- renv_install_package_options(package)
  before <- options$before.install %||% identity
  after  <- options$after.install %||% identity

  before(package)
  defer(after(package))

  # backup an existing installation of the package if it exists
  library <- renv_libpaths_active()
  destination <- file.path(library, package)
  callback <- renv_file_backup(destination)
  defer(callback())

  # normalize paths
  path <- renv_path_normalize(path, mustWork = TRUE)

  # get library path
  library <- renv_libpaths_active()

  # if a package already exists at that path, back it up first
  # this avoids problems with older versions of R attempting to
  # overwrite a pre-existing symlink
  #
  # https://github.com/rstudio/renv/issues/611
  installpath <- file.path(library, package)
  callback <- renv_file_backup(installpath)
  defer(callback())

  # if this failed for some reason, just remove it
  if (renv_file_broken(installpath))
    renv_file_remove(installpath)

  # if this is the path to an unpacked binary archive,
  # we can just copy the folder over
  isdir <- renv_file_type(path, symlinks = FALSE) == "directory"
  isbin <- renv_package_type(path, quiet = TRUE) == "binary"
  copyable <- isdir && isbin

  # shortcut via copying a binary directory if possible,
  # otherwise, install the package
  if (copyable)
    renv_file_copy(path, installpath, overwrite = TRUE)
  else
    r_cmd_install(package, path)

  # if we just installed a binary package, check that it can be loaded
  # (source packages are checked by default on install)
  withCallingHandlers(
    if (isbin) renv_install_test(package),
    error = function(err) unlink(installpath, recursive = TRUE)
  )

  # augment package metadata after install
  renv_package_augment(installpath, record)

  # return the path to the package
  invisible(installpath)

}

renv_install_test <- function(package) {

  # add escape hatch, just in case
  # (test binaries by default on Linux, but not Windows or macOS)
  enabled <- Sys.getenv("RENV_INSTALL_TEST_LOAD", unset = renv_platform_linux())
  if (!truthy(enabled))
    return(TRUE)

  # check whether we should skip installation testing
  opts <- r_cmd_install_option(package, c("install.opts", "INSTALL_opts"), FALSE)
  if (is.character(opts)) {
    flags <- unlist(strsplit(opts, "\\s+", perl = TRUE))
    if ("--no-test-load" %in% flags)
      return(TRUE)
  }

  # make sure we use the current library paths in the launched process
  rlibs <- paste(renv_libpaths_all(), collapse = .Platform$path.sep)
  renv_scope_envvars(R_LIBS = rlibs, R_LIBS_USER = "NULL", R_LIBS_SITE = "NULL")

  # also hide from user .Renviron files
  # https://github.com/wch/r-source/blob/1c0a2dc8ce6c05f68e1959ffbe6318a309277df3/src/library/tools/R/check.R#L273-L276
  renv_scope_envvars(R_ENVIRON_USER = "NULL")

  # make sure R_TESTS is unset here, just in case
  # https://github.com/wch/r-source/blob/1c0a2dc8ce6c05f68e1959ffbe6318a309277df3/src/library/tools/R/install.R#L76-L79
  renv_scope_envvars(R_TESTS = NULL)

  # the actual code we'll run in the other process
  # we use 'loadNamespace()' rather than 'library()' because some packages might
  # intentionally throw an error in their .onAttach() hooks
  # https://github.com/rstudio/renv/issues/1611
  code <- substitute({
    options(warn = 1L)
    loadNamespace(package)
  }, list(package = package))

  # write it to a tempfile
  script <- renv_scope_tempfile("renv-install-", fileext = ".R")
  writeLines(deparse(code), con = script)

  # check that the package can be loaded in a separate process
  renv_system_exec(
    command = R(),
    args    = c("--vanilla", "-s", "-f", renv_shell_path(script)),
    action  = sprintf("testing if '%s' can be loaded", package)
  )

  # return TRUE to indicate successful validation
  TRUE

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
    caution_bullets(
      "The following issues were discovered while preparing for installation:",
      text,
      "Installation of these packages may not succeed."
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

  installed <- map_chr(packages, renv_package_version)
  loaded <- map_chr(packages, renv_namespace_version)

  caution_bullets(
    c("", "The following loaded package(s) have been updated:"),
    packages[installed != loaded],
    "Restart your R session to use the new versions."
  )

  TRUE

}

renv_install_preflight_unknown_source <- function(records) {
  renv_check_unknown_source(records)
}

renv_install_preflight_permissions <- function(library) {

  # try creating and deleting a directory in the library folder
  file <- renv_scope_tempfile(".renv-write-test-", tmpdir = library)
  dir.create(file, recursive = TRUE, showWarnings = FALSE)

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
    caution_bullets(
      preamble = preamble,
      values = library,
      postamble = postamble
    )

  }
  # nocov end

  FALSE

}

renv_install_preflight <- function(project, libpaths, records) {

  library <- nth(libpaths, 1L)

  all(
    renv_install_preflight_unknown_source(records),
    renv_install_preflight_permissions(library)
  )

}

renv_install_report <- function(records, library) {
  renv_pretty_print_records(
    "The following package(s) will be installed:",
    records,
    sprintf("These packages will be installed into %s.", renv_path_pretty(library))
  )
}

renv_install_step_start <- function(action, package) {
  message <- sprintf("- %s %s ... ", action, package)
  printf(format(message, width = the$install_step_width))
}

renv_install_step_ok <- function(..., elapsed = NULL) {
  renv_report_ok(
    message = paste(..., collapse = ""),
    elapsed = elapsed
  )
}
