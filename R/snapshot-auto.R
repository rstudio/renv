
the$library_info <- NULL

the$snapshot_failed <- FALSE
the$snapshot_running <- FALSE
the$snapshot_suppressed <- FALSE

# nocov start
renv_snapshot_auto <- function(project) {

  # set some state so we know we're running
  the$snapshot_running <- TRUE
  defer(the$snapshot_running <- FALSE)

  # passed pre-flight checks; snapshot the library
  updated <- tryCatch(
    renv_snapshot_auto_impl(project),
    error = function(err) FALSE
  )

  if (updated) {
    lockfile <- renv_path_aliased(renv_lockfile_path(project))
    writef("- Automatic snapshot has updated '%s'.", lockfile)
  }

  invisible(updated)

}

renv_snapshot_auto_impl <- function(project) {

  # validation messages can be noisy; turn off for auto snapshot
  renv_scope_options(
    renv.config.snapshot.validate = FALSE,
    renv.verbose = FALSE
  )

  lockfile <- renv_paths_lockfile(project)
  old <- file.info(lockfile)$mtime

  # perform snapshot without prompting
  snapshot(project = project, prompt = FALSE)

  new <- file.info(lockfile)$mtime

  old != new

}

renv_snapshot_auto_enabled <- function(project = renv_project_get()) {

  # respect config setting
  config <- config$auto.snapshot(default = NULL)
  if (!is.null(config))
    return(config)

  # only snapshot interactively
  if (!interactive())
    return(FALSE)

  # only automatically snapshot the current project
  if (!renv_project_loaded(project))
    return(FALSE)

  # don't auto-snapshot if the project hasn't been initialized
  if (!renv_project_initialized(project = project))
    return(FALSE)

  # don't auto-snapshot if we don't have a library
  library <- renv_paths_library(project = project)
  if (!file.exists(library))
    return(FALSE)

  # don't auto-snapshot unless the active library is the project library
  if (!renv_file_same(renv_libpaths_active(), library))
    return(FALSE)

  TRUE

}

renv_snapshot_auto_update <- function(project = renv_project_get() ) {

  # check for enabled
  if (!renv_snapshot_auto_enabled(project = project))
    return(FALSE)

  # get path to project library
  libpath <- renv_paths_library(project = project)
  if (!file.exists(libpath))
    return(FALSE)

  # list files + get file info for files in project library
  info <- renv_file_info(libpath)

  # only keep relevant fields
  fields <- c("size", "mtime", "ctime")
  new <- c(info[fields])

  # update our cached info
  old <- the$library_info
  the$library_info <- new

  # if we've suppressed the next automatic snapshot, bail here
  if (the$snapshot_suppressed) {
    the$snapshot_suppressed <- FALSE
    return(FALSE)
  }

  # report if things have changed
  !is.null(old) && !identical(old, new)

}

renv_snapshot_task <- function() {

  # if the previous snapshot attempt failed, do nothing
  if (the$snapshot_failed)
    return(FALSE)

  # treat warnings as errors in this scope
  renv_scope_options(warn = 2L)

  # attempt automatic snapshot, but disable on failure
  tryCatch(
    renv_snapshot_task_impl(),
    error = function(cnd) {
      writef("Error generating automatic snapshot: %s", conditionMessage(cnd))
      writef("Automatic snapshots will be disabled. Use `renv::snapshot()` to manually update the lockfile.")
      the$snapshot_failed <- TRUE
    }
  )

}

renv_snapshot_task_impl <- function() {

  # check for active renv project
  project <- renv_project_get()
  if (is.null(project))
    return(invisible(FALSE))

  # see if library state has updated
  updated <- renv_snapshot_auto_update(project = project)
  if (!updated)
    return(invisible(FALSE))

  # library has updated; perform auto snapshot
  renv_snapshot_auto(project = project)

}

renv_snapshot_auto_suppress_next <- function() {

  # if we're currently running an automatic snapshot, then nothing to do
  if (the$snapshot_running)
    return()

  # otherwise, set the suppressed flag
  the$snapshot_suppressed <- TRUE

}

# nocov end
