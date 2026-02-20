
the$use_libpath <- NULL

#' @rdname embed
#'
#' @param ...
#'   The \R packages to be used with this script. Ignored if `lockfile` is
#'   non-`NULL`.
#'
#' @param lockfile
#'   The lockfile to use. When supplied, renv will use the packages as
#'   declared in the lockfile.
#'
#' @param library
#'   The library path into which the requested packages should be installed.
#'   When `NULL` (the default), a library path within the \R temporary
#'   directory will be generated and used. Note that this same library path
#'   will be re-used on future calls to `renv::use()`, allowing `renv::use()`
#'   to be used multiple times within a single script.
#'
#' @param isolate
#'   Boolean; should the active library paths be included in the set of library
#'   paths activated for this script? Set this to `TRUE` if you only want the
#'   packages provided to `renv::use()` to be visible on the library paths.
#'
#' @param sandbox
#'   Should the system library be sandboxed? See the sandbox documentation in
#'   [renv::config] for more details. You can also provide an explicit sandbox
#'   path if you want to configure where `renv::use()` generates its sandbox.
#'   By default, the sandbox is generated within the \R temporary directory.
#'
#' @param attach
#'   Boolean; should the set of requested packages be automatically attached?
#'   If `TRUE`, packages will be loaded and attached via a call
#'   to [library()] after install. Ignored if `lockfile` is non-`NULL`.
#'
#' @param verbose
#'   Boolean; be verbose while installing packages?
#'
#' @param repos
#'   The \R package repositories to use. When `NULL`, packages will be
#'   resolved from the renv cache, without querying any external repositories.
#'   This can be useful if you'd like to use packages that have already been
#'   cached by renv, even when no active package repositories have been
#'   configured.
#'
#' @return
#'   This function is normally called for its side effects.
#'
#' @export
use <- function(...,
                lockfile = NULL,
                library  = NULL,
                repos    = getOption("repos"),
                isolate  = TRUE,
                sandbox  = TRUE,
                attach   = FALSE,
                verbose  = TRUE)
{
  # allow use of the cache in this context
  renv_scope_options(renv.cache.linkable = TRUE)

  # treat NULL repos as an implicit request to only use the cache
  cacheonly <- is.null(repos)

  # set up sandbox if requested
  renv_use_sandbox(sandbox)

  # prepare library and activate library
  library <- library %||% renv_use_libpath()
  ensure_directory(library)

  # set library paths
  libpaths <- c(library, if (!isolate) .libPaths())
  renv_libpaths_set(libpaths)

  # override repos
  renv_scope_options(repos = repos)

  # if we were supplied a lockfile, use it
  if (!is.null(lockfile)) {
    renv_scope_options(renv.verbose = verbose)
    if (cacheonly)
      records <- renv_use_cacheonly_restore(lockfile = lockfile, library = library)
    else
      records <- restore(lockfile = lockfile, clean = FALSE, prompt = FALSE)
    return(invisible(records))
  }

  dots <- list(...)
  if (empty(dots))
    return(invisible())

  # resolve the provided remotes
  records <- lapply(dots, renv_remotes_resolve, latest = !cacheonly)
  names(records) <- map_chr(records, `[[`, "Package")

  # remove any remotes which already appear to be installed
  compat <- enum_lgl(records, function(package, record) {

    # check if the package is installed
    if (!renv_package_installed(package, lib.loc = library))
      return(FALSE)

    # check if the installed package is compatible
    record <- resolve(record)
    current <- renv_snapshot_description(package = package)
    diff <- renv_lockfile_diff_record(record, current)

    # a null diff implies the two records are compatible
    is.null(diff)

  })

  # drop the already-installed compatible records
  records <- records[!compat]
  if (empty(records))
    return(invisible())

  # install packages
  if (cacheonly) {
    records <- renv_use_cacheonly_install(records = records, library = library)
  } else {
    records <- local({
      renv_scope_options(renv.verbose = verbose)
      install(packages = records, library = library, rebuild = character(), prompt = FALSE)
    })
  }

  # automatically load the requested remotes
  if (attach) {
    enumerate(records, function(package, remote) {
      library(package, character.only = TRUE)
    })
  }

  # return set of installed packages
  invisible(records)

}

renv_use_libpath <- function() {
  (the$use_libpath <- the$use_libpath %||% tempfile("renv-use-libpath-"))
}

renv_use_cacheonly_restore <- function(lockfile, library) {

  # read the lockfile
  if (is.character(lockfile))
    lockfile <- renv_lockfile_read(lockfile)

  # apply overrides
  lockfile <- renv_lockfile_override(lockfile)

  # extract and install records from cache
  records <- renv_lockfile_records(lockfile)
  renv_use_cacheonly_install(records = records, library = library)

}

renv_use_cacheonly_install <- function(records, library) {

  # determine how to place packages from cache into library
  linkable <- getOption("renv.cache.linkable", default = FALSE)
  linker <- if (linkable) renv_file_link else renv_file_copy

  installed <- list()

  enumerate(records, function(package, record) {

    # skip base packages
    if (package %in% renv_packages_base())
      return()

    # skip packages that aren't cacheable
    if (!renv_record_cacheable(record))
      return()

    # look up package in the cache
    path <- renv_cache_find(record)

    # if the record has no version (e.g. resolved with latest = FALSE),
    # fall back to searching the cache for any available version
    if (!nzchar(path) && is.null(record$Version)) {
      paths <- renv_cache_list(packages = package)
      path <- head(paths, n = 1L) %||% ""
    }

    if (!nzchar(path) || !renv_cache_package_validate(path)) {
      writef("- Package '%s' is not available in the cache.", package)
      return()
    }

    # skip if the target is already up-to-date
    target <- file.path(library, package)
    if (renv_file_same(path, target))
      return()

    # back up any existing installation, then link / copy from cache
    callback <- renv_file_backup(target)
    defer(callback())
    linker(path, target)

    installed[[package]] <<- record

  })

  invisible(installed)

}

renv_use_sandbox <- function(sandbox) {

  if (identical(sandbox, FALSE))
    return(FALSE)

  if (renv_sandbox_activated())
    return(TRUE)

  sandbox <- if (is.character(sandbox))
    sandbox
  else
    file.path(tempdir(), "renv-sandbox")

  renv_scope_options(renv.config.sandbox.enabled = TRUE)
  renv_sandbox_activate_impl(sandbox = sandbox)

  reg.finalizer(renv_envir_self(), function(envir) {
    tryCatch(
      renv_sandbox_unlock(sandbox),
      condition = identity
    )
  }, onexit = TRUE)

}
