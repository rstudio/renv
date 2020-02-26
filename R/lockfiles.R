
#' Lockfiles
#'
#' A **lockfile** records the state of a project at some point in time.
#'
#' A lockfile captures the state of a project's library at some point in time.
#' In particular, the package names, their versions, and their sources (when
#' known) are recorded in the lockfile.
#'
#' Projects can be restored from a lockfile using the [restore()] function. This
#' implies re-installing packages into the project's private library, as encoded
#' within the lockfile.
#'
#' While lockfiles are normally generated and used with [snapshot()] /
#' [restore()], they can also hand-edited if so desired. Lockfiles are
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
#' @name lockfiles
#' @rdname lockfiles
NULL
