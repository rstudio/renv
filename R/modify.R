
#' Open the Lockfile for Editing
#'
#' Open a project's lockfile (if any) for editing. After edit, if the lockfile
#' edited is associated with the active project, any state-related changes
#' (e.g. to \R repositories) will be updated in the current session.
#'
#' @inherit renv-params
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # modify an existing lockfile
#' if (interactive())
#'   renv::modify()
#'
#' }
modify <- function(project = NULL) {
  renv_scope_error_handler()
  project <- renv_project_resolve(project)
  renv_modify_impl(project)
  invisible(project)
}

renv_modify_impl <- function(project) {

  path <- renv_lockfile_path(project)
  if (!file.exists(path))
    stopf("lockfile '%s' does not exist", aliased_path(path))

  renv_file_edit(path)

  if (!renv_path_same(project, renv_project()))
    return(NULL)

  lockfile <- catch(renv_lockfile_load(project))
  if (inherits(lockfile, "error")) {
    warning(lockfile)
    return(NULL)
  }

  repos <- lockfile$R$Repositories
  options(repos = convert(repos, "character"))

}
