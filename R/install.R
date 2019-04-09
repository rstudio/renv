
#' Install Packages
#'
#' Install one or more \R packages.
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
#' @inheritParams renv-params
#'
#' @param packages A character vector of \R packages to install. Required
#'   package dependencies (`Depends`, `Imports`, `LinkingTo`) will be installed
#'   as required.
#'
#' @param library The library from which packages should be installed. When
#'   `NULL`, the active library (that is, the first entry reported in
#'   `.libPaths()`) is used instead.
#'
#' @export
install <- function(packages,
                    project = NULL,
                    library = NULL)
{
  project <- project %||% renv_project()
  library <- library %||% renv_libpaths_default()

  if (library == renv_paths_library(project = project))
    on.exit(renv_snapshot_auto(project = project), add = TRUE)

  records <- renv_snapshot_r_packages(library = library)

  remotes <- lapply(packages, renv_remotes_parse)
  packages <- extract_chr(remotes, "Package")
  names(remotes) <- packages
  records[names(remotes)] <- remotes

  renv_restore_begin(records, packages)
  on.exit(renv_restore_end(), add = TRUE)

  # retrieve packages
  records <- renv_retrieve(packages, records)
  renv_install(project, records)

  invisible(records)
}

renv_install <- function(project, records) {

  # save active library
  library <- renv_libpaths_default()
  renv_global_set("install.library", library)
  on.exit(renv_global_clear("install.library"), add = TRUE)

  # set up a dummy library path for installation
  templib <- tempfile("renv-templib-")
  ensure_directory(templib)
  on.exit(unlink(templib), add = TRUE)
  renv_scope_libpaths(c(templib, .libPaths()))

  # figure out whether we can use the cache during install
  linkable <-
    settings$use.cache() &&
    identical(library, renv_paths_library(project = project))

  linker <- if (linkable) renv_file_link else renv_file_copy

  # iterate through records and install
  for (record in records)
    renv_install_impl(record, linker)

  # migrate packages into true library
  sources <- list.files(templib, full.names = TRUE)
  targets <- file.path(library, basename(sources))
  names(targets) <- sources
  enumerate(targets, renv_file_move, overwrite = TRUE)

}

renv_install_impl <- function(record, linker = renv_file_copy) {

  # skip installation if the requested record matches
  # the already-installed record
  if (renv_restore_skip(record))
    return(TRUE)

  # check for cache entry and install if there
  cache <- renv_cache_package_path(record)
  if (file.exists(cache))
    return(renv_install_package_cache(record, cache, linker))

  # report that we're about to start installation
  fmt <- "Installing %s [%s] from %s ..."
  with(record, vwritef(fmt, Package, Version, renv_alias(Source)))

  # otherwise, install
  status <- catch(renv_install_package_local(record))
  renv_install_report_status(record, status)

  # link into cache
  if (settings$use.cache())
    renv_cache_synchronize(record, link = identical(linker, renv_file_link))

}

renv_install_package_cache <- function(record, cache, linker) {

  if (renv_install_package_cache_skip(record, cache))
    return(TRUE)

  library <- renv_libpaths_default()
  target <- file.path(library, record$Package)

  # back up the previous installation if needed
  callback <- renv_file_scoped_backup(target)
  on.exit(callback(), add = TRUE)

  # report successful link to user
  fmt <- "Installing %s [%s] ..."
  with(record, vwritef(fmt, Package, Version))

  status <- catch(linker(cache, target))
  if (inherits(status, "error"))
    return(status)

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
  library <- renv_global_get("install.library")
  target <- file.path(library, record$Package)

  renv_file_same(cache, target)

}

renv_install_package_local <- function(record, quiet = TRUE) {

  package <- record$Package

  # get user-defined options to apply during installation
  options <- renv_install_package_options(package)

  # run user-defined hooks before, after install
  before <- options$before.install %||% identity
  after  <- options$after.install %||% identity

  before(package)
  on.exit(after(package), add = TRUE)

  library <- renv_libpaths_default()
  path <- record$Path

  destination <- file.path(library, package)
  callback <- renv_file_scoped_backup(destination)
  on.exit(callback(), add = TRUE)

  # install the package
  renv_install_package_local_impl(package, path, library)

  # augment the DESCRIPTION after install
  installpath <- file.path(library, package)
  renv_description_augment(installpath, record)

  # return the path to the package
  invisible(installpath)

}

renv_install_package_local_impl <- function(package, path, library) {
  library <- normalizePath(library, winslash = "/", mustWork = TRUE)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  r_cmd_install(package, path, library)
}

renv_install_package_options <- function(package) {
  options <- getOption("renv.install.package.options")
  options[[package]]
}

renv_install_report_status <- function(record, status) {

  if (inherits(status, "error")) {
    vwritef("\tFAILED\n")
    stop(status)
  }

  feedback <- if (endswith(record$Path, ".tar.gz"))
    "built from source"
  else
    "installed binary"

  vwritef("\tOK (%s)", feedback)

  return(TRUE)

}
