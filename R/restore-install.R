
renv_restore_install <- function(records) {

  for (record in records)
    renv_restore_install_impl(record)

}

renv_restore_install_impl <- function(record) {

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
  state <- renv_restore_state()

  # construct target install path
  library <- renv_libpaths_default()
  target <- file.path(library, record$Package)

  # determine if we should copy or link from the cache
  # (prefer copying if we're writing to a non-renv path)
  projlib <- renv_paths_library()
  ensure_directory(projlib)
  cacheable <- settings$use.cache() && path_within(target, projlib)

  # check to see if we already have an up-to-date symlink
  # into the cache (nothing to do if that's the case)
  skip <-
    cacheable &&
    !record$Package %in% state$packages &&
    renv_file_same(cache, target)
  if (skip)
    return(TRUE)

  # choose appropriate copier / linker
  link <- if (cacheable) renv_file_link else renv_file_copy

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

  destination <- file.path(lib, package)
  callback <- renv_file_scoped_backup(destination)
  on.exit(callback(), add = TRUE)

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

  if (settings$use.cache())
    renv_cache_synchronize(record)

  return(TRUE)

}
