#' @rdname activate
#' @param clean If `TRUE`, will also remove the `renv/` directory and the
#'   lockfile.
#' @export
deactivate <- function(project = NULL, clean = FALSE) {

  renv_scope_error_handler()

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  renv_infrastructure_remove_rprofile(project)

  unload(project)

  if (clean) {
    unlink(file.path(project, "renv.lock"))
    unlink(file.path(project, "renv"), recursive = TRUE, force = TRUE)
  }

  renv_restart_request(project, reason = "renv deactivated")
  invisible(project)

}
