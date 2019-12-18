
renv_lockfile_resolve <- function(lockfile) {
  UseMethod("renv_lockfile_resolve")
}

#' @export
renv_lockfile_resolve.renv_lockfile_api <- function(lockfile) {
  lockfile$data()
}

#' @export
renv_lockfile_resolve.default <- function(lockfile) {
  lockfile
}

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

  .self$add <- function(..., .list = NULL) {

    records <- renv_records(.lockfile)

    dots <- .list %||% list(...)
    enumerate(dots, function(package, remote) {
      resolved <- renv_remotes_resolve(remote)
      records[[package]] <<- resolved
    })

    renv_records(.lockfile) <<- records
    invisible(.self)

  }

  .self$remove <- function(packages) {
    records <- renv_records(.lockfile)
    records <- drop(records, packages)
    renv_records(.lockfile) <<- records
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

#' @export
print.renv_lockfile_api <- function(x, ...) {
  lockfile <- x$data()
  records <- renv_records(lockfile)
  n <- length(records)
  fmt <- "A lockfile containing %i package %s."
  msg <- sprintf(fmt, n, plural("record", n))
  writeLines(msg)
}

#' @export
str.renv_lockfile_api <- function(object, ...) {
  lockfile <- object$data()
  str(lockfile)
}

#' Programmatically Create and Modify a Lockfile
#'
#' This function provides an API for creating and modifying `renv` lockfiles.
#' This can be useful when you'd like to programmatically generate or modify
#' a lockfile -- for example, because you want to update or change a package
#' record in an existing lockfile.
#'
#' @inheritParams renv-params
#'
#' @param file The path to an existing lockfile. When no lockfile is provided,
#'   a new one will be created based on the current project context. If you
#'   want to create a blank lockfile, use `file = NA` instead.
#'
#' @seealso \code{\link{lockfiles}}, for a description of the structure of an
#'   `renv` lockfile.
#'
#' @examples
#'
#' \dontrun{
#'
#' lock <- lockfile("renv.lock")
#'
#' # set the repositories for a lockfile
#' lock$repos(CRAN = "https://cran.r-project.org")
#'
#' # depend on digest 0.6.22
#' lock$add(digest = "digest@@0.6.22")
#'
#' # write to file
#' lock$write("renv.lock")
#'
#' }
lockfile <- function(file = NULL, project = NULL) {
  project <- renv_project_resolve(project)
  renv_scope_error_handler()

  lock <- if (is.null(file)) {

    renv_lockfile_create(
      project = project,
      library = renv_libpaths_all(),
      type    = settings$snapshot.type(project = project)
    )

  } else if (is.na(file)) {

    renv_lockfile_init(project)

  } else {

    renv_lockfile_read(file = file)

  }

  renv_lockfile_api(lock)

}
