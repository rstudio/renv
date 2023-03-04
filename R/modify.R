
#' Modify a Lockfile
#'
#' Modify a project's lockfile, either interactively or non-interactively.
#'
#' After edit, if the lockfile edited is associated with the active project, any
#' state-related changes (e.g. to \R repositories) will be updated in the
#' current session.
#'
#' @inherit renv-params
#'
#' @param changes A list of changes to be merged into the lockfile.
#'   When `NULL` (the default), the lockfile is instead opened for
#'   interactive editing.
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
modify <- function(project = NULL, changes = NULL) {
  renv_scope_error_handler()
  project <- renv_project_resolve(project)
  renv_project_lock(project = project)
  renv_modify_impl(project, changes)
  invisible(project)
}

renv_modify_impl <- function(project, changes) {

  lockfile <- if (is.null(changes))
    renv_modify_interactive(project)
  else
    renv_modify_noninteractive(project, changes)

  if (identical(renv_project(), project))
    renv_modify_fini(lockfile)

}

renv_modify_interactive <- function(project) {

  # check for interactive session
  if (!interactive())
    stop("can't modify lockfile in non-interactive session")

  # resolve path to lockfile
  lockpath <- renv_lockfile_path(project)
  if (!file.exists(lockpath))
    stopf("lockfile '%s' does not exist", renv_path_aliased(lockpath))

  # copy the lockfile to a temporary file
  dir <- tempfile("renv-lockfile-")
  ensure_directory(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  templock <- file.path(dir, "renv.lock")
  file.copy(lockpath, templock)

  # edit the temporary lockfile
  renv_file_edit(templock)

  # check that the new lockfile can be read
  lockfile <- catch(renv_lockfile_read(file = templock))
  if (inherits(lockfile, "error")) {

    renv_pretty_print(
      conditionMessage(lockfile),
      preamble  = "renv was uanble to parse the modified lockfile:",
      postamble = "Your changes will be discarded.",
      wrap = FALSE
    )

    stop("error modifying lockfile")

  }

  lockfile

}

renv_modify_noninteractive <- function(project, changes) {

  # resolve path to lockfile
  lockpath <- renv_lockfile_path(project)
  if (!file.exists(lockpath))
    stopf("lockfile '%s' does not exist", renv_path_aliased(lockpath))

  # read it
  lockfile <- renv_lockfile_read(file = lockpath)

  # merge changes
  merged <- modifyList(lockfile, changes)

  # write updated lockfile to a temporary file
  templock <- tempfile("renv-lock-")
  renv_lockfile_write(merged, file = templock)

  # try reading it once more
  newlock <- renv_lockfile_read(file = templock)
  if (!identical(merged, newlock))
    stop("modify produced an invalid lockfile")

  # overwrite the original lockfile
  file.rename(templock, lockpath)

  # finish up
  merged

}

renv_modify_fini <- function(lockfile) {

  # synchronize relevant changes into the session
  repos <- lockfile$R$Repositories
  options(repos = convert(repos, "character"))

}
