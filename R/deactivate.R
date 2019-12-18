
#' Deactivate a Project
#'
#' Use `deactivate()` to remove the infrastructure used by `renv` to activate
#' projects for newly-launched \R sessions. In particular, this implies removing
#' the requisite code from the project `.Rprofile` that automatically activates
#' the project when new \R sessions are launched in the project directory.
#'
#' @inherit renv-params
#'
#' @family renv
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # deactivate the currently-activated project
#' renv::deactivate()
#'
#' }
deactivate <- function(project = NULL) {
  renv_scope_error_handler()
  project <- renv_project_resolve(project)

  renv_infrastructure_remove_rprofile(project)

  unload(project)

  renv_request_restart(project, reason = "renv deactivated")
  invisible(project)
}
