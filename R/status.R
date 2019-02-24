
#' Status
#'
#' Report differences between the project's lockfile and the current state of
#' the private library (if any).
#'
#' @inheritParams renv-params
#'
#' @export
status <- function(project = NULL) {
  project <- project %||% renv_state$project()
  invisible(renv_status(project))
}

renv_status <- function(project) {

  # ensure the project-local library is activated
  renv_scope_libpaths(renv_paths_library(project))

  # notify the user if there's no lockfile
  lockpath <- file.path(project, "renv.lock")
  if (!file.exists(lockpath)) {
    writeLines("This project has not been snapshotted yet.")
    return(NULL)
  }

  # compare the lockfile with current state of library
  old <- renv_lockfile_read(lockpath)
  new <- snapshot(file = NULL)
  renv_status_report(old, new)

}

renv_status_report <- function(old, new) {

  actions <- renv_lockfile_diff_packages(old, new)
  if (empty(actions))
    vmessagef("* The project is already synchronized with the lockfile.")

  if ("install" %in% actions) {
    msg <- "The following package(s) are installed but not recorded in the lockfile:"
    renv_pretty_print(msg, new, actions, "install")
    writeLines("Use `renv::snapshot()` to add these packages to your lockfile.")
    writeLines("")
  }

  if ("remove" %in% actions) {
    msg <- "The following package(s) are recorded in the lockfile but not installed:"
    renv_pretty_print(msg, old, actions, "remove")
    writeLines("Use `renv::restore(actions = \"install\")` to install these packages.")
    writeLines("")
  }

  rest <- c("upgrade", "downgrade", "crossgrade")
  if (any(rest %in% actions)) {
    msg <- "The following package(s) are out of sync:"
    renv_pretty_print_pair(msg, old, new, actions, rest)
    writeLines("Use `renv::snapshot()` to save the state of your library to the lockfile.")
    writeLines("Use `renv::restore()` to save the state of your library to the lockfile.")
    writeLines("")
  }

}
