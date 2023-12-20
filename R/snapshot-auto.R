
# information about the project library; used to detect whether
# the library appears to have been modified or updated
the$library_info <- NULL

# are we forcing automatic snapshots?
the$auto_snapshot_forced <- FALSE

# did the last attempt at an automatic snapshot fail?
the$auto_snapshot_failed <- FALSE

# are we currently running an automatic snapshot?
the$auto_snapshot_running <- FALSE

# is the next automatic snapshot suppressed?
the$auto_snapshot_suppressed <- FALSE

# nocov start
renv_snapshot_auto <- function(project) {

  # set some state so we know we're running
  the$auto_snapshot_running <- TRUE
  defer(the$auto_snapshot_running <- FALSE)

  # passed pre-flight checks; snapshot the library
  updated <- withCallingHandlers(

    tryCatch(
      renv_snapshot_auto_impl(project),
      error = function(err) FALSE
    ),

    cancel = function() FALSE

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

  # file.info() can warn in some cases; silence those
  renv_scope_options(warn = -1L)

  # get current lockfile state
  lockfile <- renv_paths_lockfile(project)
  old <- file.info(lockfile, extra_cols = FALSE)$mtime

  # perform snapshot without prompting
  snapshot(project = project, prompt = FALSE)

  # check for change in lockfile
  new <- file.info(lockfile, extra_cols = FALSE)$mtime
  !identical(old, new)

}

renv_snapshot_auto_enabled <- function(project = renv_project_get()) {

  # respect override
  if (the$auto_snapshot_forced)
    return(TRUE)

  # respect config setting
  enabled <- config$auto.snapshot(project = project)
  if (!enabled)
    return(FALSE)

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
  if (the$auto_snapshot_suppressed) {
    the$auto_snapshot_suppressed <- FALSE
    return(FALSE)
  }

  # report if things have changed
  !is.null(old) && !identical(old, new)

}

renv_snapshot_task <- function() {

  # if the previous snapshot attempt failed, do nothing
  if (the$auto_snapshot_failed)
    return(FALSE)

  # silence warnings in this scope
  renv_scope_options(warn = -1L)

  # attempt automatic snapshot, but disable on failure
  tryCatch(
    renv_snapshot_task_impl(),
    error = function(cnd) {
      caution("Error generating automatic snapshot: %s", conditionMessage(cnd))
      caution("Automatic snapshots will be disabled. Use `renv::snapshot()` to manually update the lockfile.")
      the$auto_snapshot_failed <- TRUE
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
  status <- renv_snapshot_auto(project = project)
  ok <- identical(status, TRUE)

  # return invisibly for snapshot tests
  invisible(ok)

}

renv_snapshot_auto_suppress_next <- function() {

  # if we're currently running an automatic snapshot, then nothing to do
  if (the$auto_snapshot_running)
    return()

  # otherwise, set the suppressed flag
  the$auto_snapshot_suppressed <- TRUE

}

# nocov end
