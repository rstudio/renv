
#' Status
#'
#' Report differences between the project's lockfile and the current state of
#' the private library (if any).
#'
#' @inheritParams renv-params
#'
#' @export
status <- function(project = NULL) {
  project <- project %||% renv_project()
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
  lock <- renv_lockfile_read(lockpath)
  curr <- snapshot(file = NULL)
  renv_status_report(lock, curr)

}

renv_status_report <- function(lock, curr) {

  actions <- renv_lockfile_diff_packages(lock, curr)
  if (empty(actions))
    vmessagef("* The project is already synchronized with the lockfile.")

  if ("install" %in% actions) {
    msg <- "The following package(s) are installed but not recorded in the lockfile:"
    renv_pretty_print(msg, curr, actions, "install")
    writeLines("Use `renv::snapshot()` to add these packages to your lockfile.")
    writeLines("")
  }

  if ("remove" %in% actions) {
    msg <- "The following package(s) are recorded in the lockfile but not installed:"
    renv_pretty_print(msg, lock, actions, "remove")
    writeLines("Use `renv::restore(actions = \"install\")` to install these packages.")
    writeLines("")
  }

  rest <- c("upgrade", "downgrade", "crossgrade")
  if (any(rest %in% actions)) {

    matches <- actions[actions %in% rest]
    data <- data.frame(
      "  Package"          = names(matches),
      "  Lockfile Version" = extract_chr(lock$R$Package[names(matches)], "Version"),
      "  Library Version"  = extract_chr(curr$R$Package[names(matches)], "Version"),
      row.names            = NULL,
      stringsAsFactors     = FALSE,
      check.names          = FALSE
    )

    writeLines("The following package(s) are out of sync:")
    writeLines("")
    print(data, row.names = FALSE, indent = "  ")
    writeLines("")
    writeLines("Use `renv::snapshot()` to save the state of your library to the lockfile.")
    writeLines("Use `renv::restore()` to restore your library from the state of the lockfile.")
    writeLines("")
  }

}
