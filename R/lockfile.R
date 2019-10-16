
#' Lockfiles
#'
#' A **lockfile** records the state of a project at some point in time. In
#' particular, this implies capturing the \R packages installed (along with
#' their versions) within the project's private library.
#'
#' Projects can be restored from a lockfile using the [restore()] function.
#' This implies re-installing packages into the project's private library,
#' as encoded within the lockfile.
#'
#' While lockfiles are normally generated and used with [snapshot()] /
#' [restore()], they can also hand-edited if so desired. Lockfiles are
#' written as `.json`, to allow for easy consumption by other tools.
#'
#' An example lockfile follows:
#'
#' ```
#' {
#'   "renv": {
#'     "Version": "1.0.0"
#'   },
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
#' @section \[renv\]:
#'
#' Information about the version of `renv` used to manage this project.
#'
#' \tabular{ll}{
#' \strong{Version}     \tab The version of the `renv` package used with this project. \cr
#' }
#'
#' @section \[R\]:
#'
#' Properties related to the version of \R associated with this project.
#'
#' \tabular{ll}{
#' \strong{Version}      \tab The version of \R used. \cr
#' \strong{Repositories} \tab The \R repositories used in this project. \cr
#' }
#'
#' @section \[Packages\]:
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
#' @section \[Python\]:
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
#' @family reproducibility
#' @name lockfile
#' @rdname lockfile
NULL

renv_lockfile_init <- function(project) {

  lockfile <- list()

  lockfile$R            <- renv_lockfile_init_r(project)
  lockfile$Python       <- renv_lockfile_init_python(project)

  class(lockfile) <- "renv_lockfile"
  lockfile

}

renv_lockfile_init_r_version <- function(project) {
  format(getRversion())
}

renv_lockfile_init_r_repos <- function(project) {

  repos <- getOption("repos")

  # save names
  nms <- names(repos)

  # force as character
  repos <- as.character(repos)

  # clear RStudio attribute
  attr(repos, "RStudio") <- NULL

  # set a default URL
  repos[repos == "@CRAN@"] <- "https://cloud.r-project.org"

  # remove RSPM bits from URL
  pattern <- "/__linux__/[^/]+/"
  repos <- sub(pattern, "/", repos)

  # force as list
  repos <- as.list(repos)

  # ensure names
  names(repos) <- nms

  repos

}

renv_lockfile_init_r <- function(project) {
  version <- renv_lockfile_init_r_version(project)
  repos   <- renv_lockfile_init_r_repos(project)
  list(Version = version, Repositories = repos)
}

renv_lockfile_init_python <- function(project) {

  python <- Sys.getenv("RENV_PYTHON", unset = NA)
  if (is.na(python))
    return(NULL)

  if (!file.exists(python))
    return(NULL)

  info <- renv_python_info(python)
  if (is.null(info))
    return(NULL)

  version <- renv_python_version(python)
  type <- info$type
  root <- info$root
  name <- renv_python_envname(project, root, type)

  list(Version = version, Type = type, Name = name)
}

renv_lockfile_path <- function(project) {
  file.path(project, "renv.lock")
}

renv_lockfile_save <- function(lockfile, project) {
  renv_lockfile_write(lockfile, file = renv_lockfile_path(project))
}

renv_lockfile_load <- function(project) {

  path <- renv_lockfile_path(project)
  if (file.exists(path))
    return(renv_lockfile_read(path))

  renv_lockfile_init(project = project)

}

renv_lockfile_sort <- function(lockfile) {

  # ensure C locale for consistent sorting
  renv_scope_locale("LC_COLLATE", "C")

  # extract R records (nothing to do if empty)
  records <- renv_records(lockfile)
  if (empty(records))
    return(lockfile)

  # sort the records
  sorted <- records[sort(names(records))]
  renv_records(lockfile) <- sorted

  # return post-sort
  lockfile

}
