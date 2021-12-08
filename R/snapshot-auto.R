
`_renv_snapshot_auto` <- new.env(parent = emptyenv())
`_renv_library_state` <- new.env(parent = emptyenv())

# nocov start
renv_snapshot_auto <- function(project) {

  # passed pre-flight checks; snapshot the library
  # validation messages can be noisy; turn off for auto snapshot
  status <- catch(renv_snapshot_auto_impl(project))
  if (inherits(status, "error"))
    return(FALSE)

  lockfile <- renv_lockfile_path(project = project)
  vwritef("* Automatic snapshot has updated '%s'.", aliased_path(lockfile))
  TRUE

}

renv_snapshot_auto_impl <- function(project) {

  # be quiet during auto snapshot
  renv_scope_options(
    renv.config.snapshot.validate = FALSE,
    renv.verbose = FALSE
  )

  # perform snapshot without prompting
  snapshot(project = project, prompt = FALSE)

}

renv_snapshot_auto_enabled <- function(project) {

  # don't auto-snapshot if disabled by user
  enabled <- config$auto.snapshot()
  if (!enabled)
    return(FALSE)

  # only automatically snapshot the current project
  if (!identical(project, renv_project(default = NULL)))
    return(FALSE)

  # don't auto-snapshot if the project hasn't been initialized
  if (!renv_project_initialized(project = project))
    return(FALSE)

  # don't auto-snapshot if we don't have a library
  library <- renv_paths_library(project = project)
  if (!file.exists(library))
    return(FALSE)

  # don't auto-snapshot unless the active library is the project library
  if (!renv_file_same(renv_libpaths_default(), library))
    return(FALSE)

  TRUE

}

renv_snapshot_auto_update <- function(project) {

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
  old <- `_renv_library_state`[["info"]]
  `_renv_library_state`[["info"]] <- new

  # if we've suppressed the next automatic snapshot, bail here
  suppressed <- `_renv_snapshot_auto`[["suppressed"]] %||% FALSE
  if (suppressed) {
    `_renv_snapshot_auto`[["suppressed"]] <- FALSE
    return(FALSE)
  }

  # report if things have changed
  !is.null(old) && !identical(old, new)

}

renv_snapshot_auto_callback <- function(...) {
  renv_snapshot_auto_callback_impl()
  TRUE
}

renv_snapshot_auto_callback_impl <- function() {

  # check for active renv project
  project <- Sys.getenv("RENV_PROJECT", unset = NA)
  if (is.na(project))
    return(FALSE)

  # see if library state has updated
  updated <- renv_snapshot_auto_update(project = project)
  if (!updated)
    return(FALSE)

  # library has updated; perform auto snapshot
  renv_snapshot_auto(project = project)
  TRUE

}

renv_snapshot_auto_suppress_next <- function() {
  `_renv_snapshot_auto`[["suppressed"]] <- TRUE
}

# nocov end
