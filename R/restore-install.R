
renv_restore_install <- function(record) {

  # check for cache entry and install if there
  cache <- renv_cache_package_path(record)
  if (file.exists(cache))
    return(renv_restore_install_package_cache(record, cache))

  # report that we're about to start installation
  fmt <- "Installing %s [%s] from %s ..."
  with(record, messagef(fmt, Package, Version, renv_alias(Source)))

  # otherwise, install
  status <- tryCatch(
    renv_restore_install_package_local(record),
    condition = identity
  )

  renv_restore_install_report_status(record, status)

}

renv_restore_install_package_cache <- function(record, cache) {

  # construct target install path
  library <- case(
    is.null(record$Library)       ~ renv_libpaths_default(),
    path_absolute(record$Library) ~ record$Library,
    TRUE                          ~ renv_paths_library(record$Library)
  )
  target <- file.path(library, record$Package)

  # determine if we should copy or link from the cache
  # (prefer copying if we're writing to a non-renv path)
  ensure_directory(renv_paths_library())
  link <- if (path_within(target, renv_paths_library()))
    renv_file_link
  else
    renv_file_copy

  # back up the previous installation if needed
  callback <- renv_file_scoped_backup(target)
  on.exit(callback(), add = TRUE)

  # now, try to hydrate from cache
  status <- catch(link(cache, target))
  if (!identical(status, TRUE))
    return(status)

  # report successful link to user
  fmt <- "Installing %s [%s] ..."
  with(record, messagef(fmt, Package, Version))

  type <- case(
    identical(link, renv_file_copy) ~ "copied",
    identical(link, renv_file_link) ~ "linked"
  )

  messagef("\tOK (%s cache)", type)

  return(TRUE)
}

renv_restore_install_package_local <- function(record) {

  package <- record$Package

  # get user-defined options to apply during installation
  options <- renv_restore_install_package_options(package)

  # run user-defined hooks before, after install
  before <- options$before.install %||% identity
  after  <- options$after.install %||% identity

  before(package)
  on.exit(after(package), add = TRUE)

  lib <- renv_libpaths_default()
  path <- record$Path
  type <- record$Type
  install.packages(

    pkgs  = path,
    lib   = lib,
    repos = NULL,
    type  = type,
    quiet = TRUE,

    configure.args = options$configure.args,
    configure.vars = options$configure.vars,
    INSTALL_opts   = options$install.options

  )

  # check to see if installation succeeded
  if (!file.exists(file.path(lib, package)))
    stopf("installation of package '%s' failed", package)

}

renv_restore_install_package_options <- function(package) {
  options <- getOption("renv.install.package.options")
  options[[package]]
}

renv_restore_install_unknown_source <- function(record) {
  fmt <- "can't restore package '%s': '%s' is an unrecognized source."
  stopf(fmt, record$Package, record$Source, call. = FALSE)
}

renv_restore_install_report_status <- function(record, status) {

  if (inherits(status, "error")) {
    message("\tFAILED")
    return(status)
  }

  type <- record$Type
  feedback <- case(
    type == "source" ~ "built from source",
    type == "binary" ~ "installed binary"
  )

  messagef("\tOK (%s)", feedback)
  renv_cache_synchronize(record)

  return(TRUE)

}
