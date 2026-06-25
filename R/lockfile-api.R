
# NOTE: These functions are used by the 'dockerfiler' package, even though
# they are not exported. We retain these functions here just to avoid issues
# during CRAN submission. We'll consider removing them in a future release.

renv_lockfile_api <- function(lockfile = NULL) {

  .lockfile <- lockfile
  .self <- new.env(parent = emptyenv())

  .self$repos <- function(..., .repos = NULL) {

    if (nargs() == 0) {
      repos <- .lockfile$R$Repositories
      return(repos)
    }

    repos <- .repos %||% list(...)
    if (is.null(names(repos)) || "" %in% names(repos))
      stop("repositories must all be named", call. = FALSE)

    .lockfile$R$Repositories <<- as.list(convert(repos, "character"))
    invisible(.self)

  }

  .self$version <- function(..., .version = NULL) {

    if (nargs() == 0) {
      version <- .lockfile$R$Version
      return(version)
    }

    version <- .version %||% c(...)

    if (length(version) > 1) {
      stop("Version should be length 1 character e.g. `\"3.6.3\"`")
    }

    .lockfile$R$Version <<- version
    invisible(.self)

  }

  .self$add <- function(..., .list = NULL) {

    records <- renv_lockfile_records(.lockfile)

    dots <- .list %||% list(...)
    enumerate(dots, function(package, remote) {
      resolved <- renv_remotes_resolve(remote)
      records[[package]] <<- resolved
    })

    renv_lockfile_records(.lockfile) <<- records
    invisible(.self)

  }

  .self$remove <- function(packages) {
    records <- renv_lockfile_records(.lockfile) %>% omit(packages)
    renv_lockfile_records(.lockfile) <<- records
    invisible(.self)
  }

  .self$write <- function(file = stdout()) {
    renv_lockfile_write(.lockfile, file = file)
    invisible(.self)
  }

  .self$data <- function() {
    .lockfile
  }

  class(.self) <- "renv_lockfile_api"
  .self

}

#' Create a lockfile
#'
#' Create an `renv` lockfile from a variety of sources.
#'
#' `lockfile()` provides a generic entry point for producing an `renv`
#' lockfile. The kind of lockfile produced depends on the value supplied as
#' `from`; for example, given the path to a Posit Connect `manifest.json`
#' file, `lockfile()` will convert that manifest into a lockfile.
#'
#' By default the lockfile is returned as an \R object. Supply `to` to also
#' write it to disk, after which [restore()] can be used to restore the
#' associated project library.
#'
#' @inheritParams renv-params
#'
#' @param from
#'   The source from which the lockfile should be created. This can be:
#'
#'   - `NULL` (the default), in which case the lockfile is created from the
#'     state of the active project's library;
#'
#'   - The path to a Posit Connect `manifest.json` file, or a manifest that
#'     has already been read in as an \R list, in which case the manifest is
#'     converted into a lockfile.
#'
#'   The set of supported sources may be expanded in future releases of
#'   `renv`.
#'
#' @param to
#'   The path to a lockfile to be written. When `NULL` (the default), the
#'   lockfile is returned as an \R object and not written to disk; otherwise,
#'   the lockfile is written to `to` and returned invisibly.
#'
#' @return
#'   An `renv` lockfile, as an \R object of class `renv_lockfile`. When `to`
#'   is supplied, the lockfile is returned invisibly.
#'
#' @seealso \code{\link{lockfiles}}, for a description of the structure of an
#'   `renv` lockfile.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # create a lockfile from a Posit Connect 'manifest.json' file
#' lock <- lockfile(from = "manifest.json")
#'
#' # convert a 'manifest.json' file and write 'renv.lock' in a single call
#' lockfile(from = "manifest.json", to = "renv.lock")
#'
#' }
lockfile <- function(from = NULL, to = NULL, project = NULL) {
  renv_scope_error_handler()
  project <- renv_project_resolve(project)

  lock <- renv_lockfile_from(from, project = project)

  if (is.null(to))
    return(lock)

  renv_lockfile_write(lock, file = to)
  invisible(lock)
}

renv_lockfile_from <- function(from = NULL, project = NULL) {

  # with no explicit source, build a lockfile from the current project library
  if (is.null(from)) {
    return(renv_lockfile_create(
      project  = project,
      libpaths = renv_libpaths_all(),
      type     = settings$snapshot.type(project = project)
    ))
  }

  # read file paths into an R object; accept an already-read list as-is
  data <- case(
    is.character(from) ~ renv_json_read(from),
    is.list(from)      ~ from,
    TRUE               ~ renv_type_unexpected(from)
  )

  # detect the kind of source we were given, and convert appropriately
  if (renv_manifest_is(data))
    return(renv_lockfile_from_manifest(data, project = project))

  stopf("don't know how to create a lockfile from an object of class '%s'", class(data)[[1L]])

}
