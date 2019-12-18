#' Generate renv Project Infrastructure
#'
#' Write the `renv` project infrastructure for a project.
#'
#' The function will update the project `.Rprofile` so that `renv` is
#' automatically loaded for new \R sessions launched in this project. `renv`
#' will also be installed and made available within the project's private
#' library.
#'
#' @inheritParams renv-params
#'
#' @param version The version of `renv` to associate with this project. By
#'   default, the version of `renv` currently installed is used.
#'
#' @export
scaffold <- function(project = NULL, version = NULL) {
  renv_scope_error_handler()
  project <- renv_project_resolve(project)
  renv_bootstrap_impl(project, version)
  renv_infrastructure_write(project, version)
  fmt <- "* renv infrastructure has been generated for project %s."
  vwritef(fmt, renv_path_pretty(project))
  invisible(project)
}
