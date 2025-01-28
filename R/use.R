
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
#' @return
#'   This function is normally called for its side effects.
#'
#' @export
use <- function(...,
                lockfile = NULL,
                library  = NULL,
                isolate  = TRUE,
                sandbox  = TRUE,
                attach   = FALSE,
                verbose  = TRUE)
{

  # allow use of the cache in this context
  renv_scope_options(renv.cache.linkable = TRUE)

  # set up sandbox if requested
  renv_use_sandbox(sandbox)

  # prepare library and activate library
  library <- library %||% renv_use_libpath()
  ensure_directory(library)

  # set library paths
  libpaths <- c(library, if (!isolate) .libPaths())
  renv_libpaths_set(libpaths)

  # if we were supplied a lockfile, use it
  if (!is.null(lockfile)) {
    renv_scope_options(renv.verbose = verbose)
    records <- restore(lockfile = lockfile, clean = FALSE, prompt = FALSE)
    return(invisible(records))
  }

  dots <- list(...)
  if (empty(dots))
    return(invisible())

  # resolve the provided remotes
  records <- lapply(dots, renv_remotes_resolve, latest = TRUE)
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
  records <- local({
    renv_scope_options(renv.verbose = verbose)
    install(packages = records, library = library, rebuild = character(), prompt = FALSE)
  })

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
