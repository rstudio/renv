
#' Rebuild the Packages in your Project Library
#'
#' Rebuild and reinstall packages in your library. This can be useful as a
#' diagnostic tool -- for example, if you find that one or more of your
#' packages fail to load, and you want to ensure that you are starting from a
#' clean slate.
#'
#' Note that binaries will be used when appropriate and available for your
#' platform. If you'd like to force packages to be rebuilt from sources, you
#' can set `options(pkgType = "source")`.
#'
#' @inherit renv-params
#'
#' @param packages The package(s) to be rebuilt. When `NULL`, all packages
#'   in the library will be installed.
#'
#' @param recursive Boolean; should dependencies of packages be rebuilt
#'   recursively? Defaults to `TRUE`.
#'
#' @return A named list of package records which were installed by `renv`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # rebuild the 'dplyr' package + all of its dependencies
#' renv::rebuild("dplyr", recursive = TRUE)
#'
#' # rebuild only 'dplyr'
#' renv::rebuild("dplyr", recursive = FALSE)
#'
#' }
rebuild <- function(packages  = NULL,
                    recursive = TRUE,
                    ...,
                    prompt  = interactive(),
                    library = NULL,
                    project = NULL)
{
  renv_consent_check()
  renv_scope_error_handler()
  renv_dots_check(...)

  project <- renv_project_resolve(project)
  library <- library %||% renv_libpaths_all()

  # get collection of packages currently installed
  records <- renv_snapshot_r_packages(library = library, project = project)
  if (empty(records)) {
    vwritef("* There are no packages currently installed -- nothing to rebuild.")
    return(invisible(records))
  }

  # subset packages based on user request
  packages <- packages %||% names(records)
  records <- records[packages]
  records <- renv_records_override(records)

  # notify the user
  preamble <- if (recursive)
    "The following package(s) and their dependencies will be reinstalled:"
  else
    "The following package(s) will be reinstalled:"

  renv_pretty_print_records(records, preamble)

  if (prompt && !proceed()) {
    message("Operation aborted.")
    return(invisible(records))
  }

  # figure out rebuild parameter
  rebuild <- if (recursive) NA else packages

  # perform the install
  install(records, library = library, project = project, rebuild = rebuild)
}
