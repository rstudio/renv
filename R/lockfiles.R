
#' Lockfiles
#'
#' A **lockfile** records the state of a project at some point in time.
#'
#' A lockfile captures the state of a project's library at some point in time.
#' In particular, the package names, their versions, and their sources (when
#' known) are recorded in the lockfile.
#'
#' Projects can be restored from a lockfile using the [restore()] function. This
#' implies reinstalling packages into the project's private library, as encoded
#' within the lockfile.
#'
#' While lockfiles are normally generated and used with [snapshot()] /
#' [restore()], they can also be edited by hand if so desired. Lockfiles are
#' written as `.json`, to allow for easy consumption by other tools.
#'
#' An example lockfile follows:
#'
#' ```
#' {
#'   "R": {
#'     "Version": "3.6.1",
#'     "Repositories": [
#'       {
#'         "Name": "CRAN",
#'         "URL": "https://cloud.r-project.org"
#'       }
#'     ]
#'   },
#'   "Packages": {
#'     "markdown": {
#'       "Package": "markdown",
#'       "Version": "1.0",
#'       "Source": "Repository",
#'       "Repository": "CRAN",
#'       "Hash": "4584a57f565dd7987d59dda3a02cfb41"
#'     },
#'     "mime": {
#'       "Package": "mime",
#'       "Version": "0.7",
#'       "Source": "Repository",
#'       "Repository": "CRAN",
#'       "Hash": "908d95ccbfd1dd274073ef07a7c93934"
#'     }
#'   }
#' }
#' ```
#'
#' The sections used within a lockfile are described next.
#'
#' ## renv
#'
#' Information about the version of renv used to manage this project.
#'
#' \tabular{ll}{
#' \strong{Version}     \tab The version of the renv package used with this project. \cr
#' }
#'
#' ## R
#'
#' Properties related to the version of \R associated with this project.
#'
#' \tabular{ll}{
#' \strong{Version}      \tab The version of \R used. \cr
#' \strong{Repositories} \tab The \R repositories used in this project. \cr
#' }
#'
#' ## Packages
#'
#' \R package records, capturing the packages used or required by a project
#' at the time when the lockfile was generated.
#'
#' \tabular{ll}{
#' \strong{Package}      \tab The package name. \cr
#' \strong{Version}      \tab The package version. \cr
#' \strong{Source}       \tab The location from which this package was retrieved. \cr
#' \strong{Repository}   \tab The name of the repository (if any) from which this package was retrieved. \cr
#' \strong{Hash}         \tab (Optional) A unique hash for this package, used for package caching. \cr
#' }
#'
#' Additional remote fields, further describing how the package can be
#' retrieved from its corresponding source, will also be included as
#' appropriate (e.g. for packages installed from GitHub).
#'
#' ## Python
#'
#' Metadata related to the version of Python used with this project (if any).
#'
#' \tabular{ll}{
#' \strong{Version} \tab The version of Python being used. \cr
#' \strong{Type}    \tab The type of Python environment being used ("virtualenv", "conda", "system") \cr
#' \strong{Name}    \tab The (optional) name of the environment being used.
#' }
#'
#' Note that the `Name` field may be empty. In that case, a project-local Python
#' environment will be used instead (when not directly using a system copy of Python).
#'
#' # Caveats
#'
#' These functions are primarily intended for expert users -- in most cases,
#' [snapshot()] and [restore()] are the primariy tools you will need when
#' creating and using lockfiles.
#'
#' @inheritParams snapshot
#' @inheritParams renv-params
#'
#' @param lockfile An `renv` lockfile; typically created by either
#'   `lockfile_create()` or `lockfile_read()`.
#'
#' @param file A file path, or \R connection.
#'
#' @family reproducibility
#' @name lockfiles
#' @rdname lockfiles
NULL

#' @param libpaths The library paths to be used when generating the lockfile.
#' @rdname lockfiles
#' @export
lockfile_create <- function(type = settings$snapshot.type(project = project),
                            libpaths = .libPaths(),
                            packages = NULL,
                            exclude = NULL,
                            prompt = interactive(),
                            force = FALSE,
                            ...,
                            project = NULL)
{
  project <- renv_project_resolve(project)
  renv_dots_check(...)

  renv_lockfile_create(
    project  = project,
    type     = type,
    libpaths = libpaths,
    packages = packages,
    exclude  = exclude,
    prompt   = prompt,
    force    = force
  )
}

#' @rdname lockfiles
#' @export
lockfile_read <- function(file = NULL, ..., project = NULL) {
  project <- renv_project_resolve(project)
  file <- file %||% renv_paths_lockfile(project = project)
  renv_lockfile_read(file = file)
}

#' @rdname lockfiles
#' @export
lockfile_write <- function(lockfile, file = NULL, ..., project = NULL) {
  project <- renv_project_resolve(project)
  file <- file %||% renv_paths_lockfile(project = project)
  renv_lockfile_write(lockfile, file = file)
}

#' @param remotes An \R vector of remote specifications.
#'
#' @param repos A named vector, mapping \R repository names to their URLs.
#'
#' @rdname lockfiles
#' @export
lockfile_modify <- function(lockfile = NULL,
                            ...,
                            remotes = NULL,
                            repos = NULL,
                            project = NULL)
{
  renv_dots_check(...)

  project <- renv_project_resolve(project)
  lockfile <- lockfile %||% renv_lockfile_load(project, strict = TRUE)

  if (!is.null(repos))
    lockfile$R$Repositories <- as.list(repos)

  if (!is.null(remotes)) {
    remotes <- renv_records_resolve(remotes, latest = TRUE)
    names(remotes) <- map_chr(remotes, `[[`, "Package")
    enumerate(remotes, function(package, remote) {
      record <- renv_remotes_resolve(remote)
      renv_lockfile_records(lockfile)[[package]] <<- record
    })
  }

  lockfile

}
