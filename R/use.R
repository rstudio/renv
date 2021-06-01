
#' Use a set of Packages
#'
#' Given a set of \R package requirements, install those packages into the
#' library path requested via `library`, and then activate that library path.
#'
#' `renv::use()` is intended to be used within standalone \R scripts. It can
#' be useful when you'd like to specify an \R script's dependencies directly
#' within that script, and have those packages automatically installed and
#' loaded when the associated script is run. In this way, an \R script can more
#' easily be shared and re-run with the exact package versions requested via
#' `use()`.
#'
#' `renv::use()` is inspired in part by the [groundhog](https://groundhogr.com/)
#' package, which provides an alternate mechanism for specifying a script's
#' \R package requirements within that same \R script.
#'
#' @param ...
#'   The \R packages to be used with this script. Ignored if `lockfile` is
#'   non-`NULL`.
#'
#' @param lockfile
#'   The lockfile to use. When supplied, `renv` will use the packages as
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
                isolate  = FALSE,
                attach   = FALSE,
                verbose  = TRUE)
{

  # allow use of the cache in this context
  renv_scope_options(renv.cache.linkable = TRUE)

  # prepare library and activate library
  library <- library %||% renv_use_libpath()
  ensure_directory(library)

  # set library paths
  libpaths <- c(library, if (!isolate) .libPaths())
  renv_libpaths_set(libpaths)

  # if we were supplied a lockfile, use it
  if (!is.null(lockfile)) {
    renv_scope_options(renv.verbose = verbose)
    records <- restore(lockfile = lockfile, clean = FALSE)
    return(invisible(records))
  }

  dots <- list(...)
  if (empty(dots))
    return(invisible())

  # resolve the provided remotes
  remotes <- lapply(dots, renv_remotes_resolve)
  names(remotes) <- map_chr(remotes, `[[`, "Package")

  fmt <- "* renv is installing %i package(s) and their dependencies ... "
  vprintf(fmt, length(remotes))

  # install packages
  records <- local({
    renv_scope_options(renv.verbose = verbose)
    install(packages = remotes, library = library, prompt = FALSE)
  })

  vwritef("Done!")

  # automatically load the requested remotes
  if (attach) {
    enumerate(remotes, function(package, remote) {
      library(package, character.only = TRUE)
    })
  }

  # return set of installed packages
  invisible(records)

}

renv_use_libpath <- function() {
  renv_global("use.library", tempfile("renv-library-"))
}
