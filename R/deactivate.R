#' @rdname activate
#' @export
deactivate <- function(project = NULL) {

  renv_scope_error_handler()

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  renv_infrastructure_remove_rprofile(project)

  unload(project)

  renv_restart_request(project, reason = "renv deactivated")
  invisible(project)

}
