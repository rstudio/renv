
#' View Lockfile History
#'
#' Use your version control system to find prior versions of the `renv.lock`
#' file that have been used in your project.
#'
#' The `history()` function is currently only implemented for projects using
#' `git` for version control.
#'
#' @inheritParams renv-params
#'
#' @export
history <- function(project = NULL) {
  renv_scope_error_handler()
  project <- project %||% renv_project()

  lockpath <- renv_lockfile_path(project)
  if (!file.exists(lockpath))
    return(data.frame())

  renv_git_preflight()

  arguments <- c("log", "--pretty=format:'%H\031%at\031%ct\031%s'", shQuote(lockpath))
  data <- system2("git", arguments, stdout = TRUE)
  parts <- strsplit(data, "\031", fixed = TRUE)
  tbl <- bind_list(parts, names = c("commit", "author_date", "committer_date", "subject"))
  tbl$author_date <- as.POSIXct(as.numeric(tbl$author_date), origin = "1970-01-01")
  tbl$committer_date <- as.POSIXct(as.numeric(tbl$committer_date), origin = "1970-01-01")

  tbl
}

#' Revert Lockfile
#'
#' Revert the lockfile to its contents at a prior commit.
#'
#' The `revert()` function is currently only implemented for projects using
#' `git` for version control.
#'
#' @inheritParams renv-params
#'
#' @param commit The commit associated with a prior version of the lockfile.
#' @param ... Optional arguments; currently unused.
#'
#' @export
revert <- function(commit = "HEAD", ..., project = NULL) {
  renv_scope_error_handler()
  project <- project %||% renv_project()
  lockpath <- renv_lockfile_path(project)
  renv_git_preflight()
  system2("git", c("checkout", commit, "--", shQuote(lockpath)))
  system2("git", c("reset", "HEAD", shQuote(lockpath)), stdout = FALSE, stderr = FALSE)
  system2("git", c("diff", "--", shQuote(lockpath)))
  vwritef("* renv.lock from commit %s has been checked out.", commit)
  invisible(commit)
}
