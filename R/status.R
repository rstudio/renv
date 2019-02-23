
status <- function(project = NULL) {
  project <- project %||% renv_state$project()
  invisible(renv_status(project))
}

renv_status <- function(project) {

  # ensure library paths set for context of call
  libpaths <- .libPaths()
  renv_load_libpaths(project = project)
  on.exit(.libPaths(libpaths), add = TRUE)

  # notify the user if there's no lockfile
  lockpath <- file.path(project, "renv.lock")
  if (!file.exists(lockpath)) {
    writeLines("This project has not been snapshotted yet.")
    return(NULL)
  }

  # compare the lockfile with current state of library
  saved   <- renv_lockfile_read(lockpath)
  current <- snapshot(file = NULL)

}
