
#' View and revert to a historical lockfile
#'
#' @description
#' `history()` uses your version control system to show prior versions of the
#' lockfile and `revert()` allows you to restore one of them.
#'
#' These functions are currently only implemented for projects that use git.
#'
#' @inherit renv-params
#'
#' @export
#'
#' @return `history()` returns a `data.frame` summarizing the commits in which
#'   `renv.lock` has been changed. `revert()` is usually called for its
#'   side-effect but also invisibly returns the `commit` used.
#'
#' @examples
#' \dontrun{
#'
#' # get history of previous versions of renv.lock in VCS
#' db <- renv::history()
#'
#' # choose an older commit
#' commit <- db$commit[5]
#'
#' # revert to that version of the lockfile
#' renv::revert(commit = commit)
#'
#' }
history <- function(project = NULL) {

  renv_scope_error_handler()

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  lockpath <- renv_lockfile_path(project)
  if (!file.exists(lockpath))
    return(data_frame())

  renv_git_preflight()

  renv_scope_wd(project)

  args <- c("log", "--pretty=format:%H\031%at\031%ct\031%s", renv_shell_path(lockpath))
  data <- renv_system_exec("git", args, action = "retrieving git log")

  parts <- strsplit(data, "\031", fixed = TRUE)
  tbl <- bind(parts, names = c("commit", "author_date", "committer_date", "subject"))
  tbl$author_date <- as.POSIXct(as.numeric(tbl$author_date), origin = "1970-01-01")
  tbl$committer_date <- as.POSIXct(as.numeric(tbl$committer_date), origin = "1970-01-01")

  tbl

}

#' @param commit The commit associated with a prior version of the lockfile.
#' @param ... Optional arguments; currently unused.
#' @export
#' @rdname history
revert <- function(commit = "HEAD", ..., project = NULL) {

  renv_scope_error_handler()
  renv_dots_check(...)

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  renv_git_preflight()

  renv_scope_wd(project)

  lockpath <- renv_lockfile_path(project = project)
  system2("git", c("checkout", commit, "--", renv_shell_path(lockpath)))
  system2("git", c("reset", "HEAD", renv_shell_path(lockpath)), stdout = FALSE, stderr = FALSE)
  system2("git", c("diff", "--", renv_shell_path(lockpath)))

  writef("- renv.lock from commit %s has been checked out.", commit)
  invisible(commit)

}
