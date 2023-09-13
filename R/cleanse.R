
# tools for cleaning up renv's cached data
cleanse <- function() {

  enabled <- Sys.getenv("RENV_CLEANSE_ENABLED", unset = "TRUE")
  if (!truthy(enabled))
    return(invisible(FALSE))

  # remove unused sandbox directories
  renv_cleanse_sandbox(path = renv_paths_sandbox())

  # remove empty directories in the root directory
  # we can't do this on Windows, as some empty directories
  # might also be broken junctions, and we want to keep
  # those around so we can inform the user that they need
  # to repair that
  if (!renv_platform_windows())
    renv_cleanse_empty(path = renv_paths_root())

  invisible(TRUE)

}

renv_cleanse_sandbox <- function(path) {

  # get sandbox root path
  root <- dirname(path)
  if (!file.exists(root))
    return(FALSE)

  # list directories within
  dirs <- list.files(root, full.names = TRUE)

  # look for apparently-unused sandbox directories
  info <- suppressWarnings(file.info(dirs, extra_cols = FALSE))
  age <- difftime(Sys.time(), info$mtime, units = "days")
  old <- age >= 7

  # remove the old sandbox directories
  unlink(dirs[old], recursive = TRUE, force = TRUE)

}

renv_cleanse_empty <- function(path) {

  # no-op for Solaris
  if (renv_platform_solaris())
    return(FALSE)

  if (!file.exists(path))
    return(FALSE)

  renv_scope_wd(path)

  # execute system command for removing empty directories
  action <- "removing empty directories"
  if (renv_platform_windows()) {
    args <- c(".", ".", "/S", "/MOVE")
    renv_system_exec("robocopy", args, action, 0:8)
  } else {
    args <- c(".", "-type", "d", "-empty", "-delete")
    renv_system_exec("find", args, action)
  }

  TRUE

}
