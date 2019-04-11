
#' Open the Lockfile for Editing
#'
#' Open a project's lockfile (if any) for editing. After edit, if the lockfile
#' edited is associated with the active project, any state-related changes
#' (e.g. to \R repositories) will be updated in the current session.
#'
#' @inheritParams renv-params
#'
#' @export
modify <- function(project = NULL) {
  renv_scope_error_handler()
  project <- project %||% renv_project()
  invisible(renv_modify_impl(project))
}

renv_modify_impl <- function(project) {

  path <- file.path(project, "renv.lock")
  if (!file.exists(path))
    stopf("lockfile '%s' does not exist", aliased_path(path))

  renv_file_edit(path)

  if (!path_same(project, renv_project()))
    return(NULL)

  lockfile <- catch(renv_lockfile_load(project))
  if (inherits(lockfile, "error")) {
    warning(lockfile)
    return(NULL)
  }

  options(repos = lockfile$R$Repositories)
  NULL

}
