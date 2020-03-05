
#' View Lockfile History
#'
#' Use your version control system to find prior versions of the `renv.lock`
#' file that have been used in your project.
#'
#' The `history()` function is currently only implemented for projects using
#' `git` for version control.
#'
#' @inherit renv-params
#'
#' @export
#'
#' @return An \R `data.frame`, summarizing the commits in which `renv.lock`
#'   has been mutated.
#'
#' @example examples/examples-history.R
history <- function(project = NULL) {

  renv_scope_error_handler()
  project <- renv_project_resolve(project)

  lockpath <- renv_lockfile_path(project)
  if (!file.exists(lockpath))
    return(data.frame())

  renv_git_preflight()

  owd <- setwd(project)
  on.exit(setwd(owd), add = TRUE)

  arguments <- c("log", "--pretty=format:%H\031%at\031%ct\031%s", "renv.lock")
  data <- system2("git", shQuote(arguments), stdout = TRUE)

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
#' @inherit renv-params
#'
#' @param commit The commit associated with a prior version of the lockfile.
#' @param ... Optional arguments; currently unused.
#'
#' @return The commit used when reverting `renv.lock`. Note that this function
#'   is normally called for its side effects.
#'
#' @export
#'
#' @example examples/examples-history.R
revert <- function(commit = "HEAD", ..., project = NULL) {

  renv_scope_error_handler()
  renv_dots_check(...)
  project <- renv_project_resolve(project)

  renv_git_preflight()

  owd <- setwd(project)
  on.exit(setwd(owd), add = TRUE)

  system2("git", c("checkout", commit, "--", "renv.lock"))
  system2("git", c("reset", "HEAD", "renv.lock"), stdout = FALSE, stderr = FALSE)
  system2("git", c("diff", "--", "renv.lock"))

  vwritef("* renv.lock from commit %s has been checked out.", commit)
  invisible(commit)

}
