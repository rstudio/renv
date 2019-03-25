
renv_restore_install <- function(project, records) {

  # save active library
  library <- renv_libpaths_default()

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
    renv_restore_install_impl(record, linker)

  # migrate packages into true library
  sources <- list.files(templib, full.names = TRUE)
  targets <- file.path(library, basename(sources))
  names(targets) <- sources
  enumerate(targets, renv_file_move, overwrite = TRUE)

}

renv_restore_install_impl <- function(record, linker = renv_file_copy) {

  # check for cache entry and install if there
  cache <- renv_cache_package_path(record)
  if (file.exists(cache))
    return(renv_restore_install_package_cache(record, cache, linker))

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

renv_restore_install_package_cache <- function(record, cache, linker) {

  state <- renv_restore_state()

  # construct target install path
  library <- renv_libpaths_default()
  target <- file.path(library, record$Package)

  # check to see if we already have an up-to-date symlink into the cache
  # (nothing to do if that's the case)
  skip <-
    !record$Package %in% state$packages &&
    renv_file_same(cache, target)

  if (skip)
    return(TRUE)

  # back up the previous installation if needed
  callback <- renv_file_scoped_backup(target)
  on.exit(callback(), add = TRUE)

  # report successful link to user
  fmt <- "Installing %s [%s] ..."
  with(record, messagef(fmt, Package, Version))

  status <- catch(linker(cache, target))
  if (inherits(status, "error"))
    return(status)

  type <- case(
    identical(linker, renv_file_copy) ~ "copied",
    identical(linker, renv_file_link) ~ "linked"
  )

  messagef("\tOK (%s cache)", type)

  return(TRUE)

}

renv_restore_install_package_local <- function(record, quiet = TRUE) {

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
  type <- record$Type %||% renv_package_type(path)

  destination <- file.path(lib, package)
  callback <- renv_file_scoped_backup(destination)
  on.exit(callback(), add = TRUE)

  install.packages(

    pkgs  = path,
    lib   = lib,
    repos = NULL,
    type  = type,
    quiet = quiet,

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
