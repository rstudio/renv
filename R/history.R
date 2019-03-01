
#' View Lockfile History
#'
#' The `history()` function can be used to search for prior versions of the
#' `renv.lock` file used within a project.
#'
#' The `history()` function is currently only implemented for projects using
#' `git` for version control.
#'
#' @inheritParams renv-params
#'
#' @export
history <- function(project = NULL) {
  project <- project %||% renv_project()

  lockpath <- file.path(project, "renv.lock")
  if (!file.exists(lockpath))
    return(data.frame())

  renv_git_preflight()

  arguments <- c("log", "--pretty=format:'%H\031%at\031%ct\031%s'", shQuote(lockpath))
  data <- system2("git", arguments, stdout = TRUE)

  tbl <- read.table(text = data, sep = "\031", row.names = NULL, header = FALSE, stringsAsFactors = FALSE)
  names(tbl) <- c("commit", "author_date", "committer_date", "subject")
  tbl$author_date <- as.POSIXct(tbl$author_date, origin = "1970-01-01")
  tbl$committer_date <- as.POSIXct(tbl$committer_date, origin = "1970-01-01")

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
#'
#' @export
revert <- function(project = NULL, commit = "HEAD") {
  project <- project %||% renv_project()
  lockpath <- file.path(project, "renv.lock")
  renv_git_preflight()
  system2("git", c("checkout", commit, "--", shQuote(lockpath)))
  system2("git", c("reset", "HEAD", shQuote(lockpath)), stdout = FALSE, stderr = FALSE)
  system2("git", c("diff", "--", shQuote(lockpath)))
}
