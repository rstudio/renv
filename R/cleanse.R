
# tools for cleaning up renv's cached data
cleanse <- function() {

  enabled <- Sys.getenv("RENV_CLEANSE_ENABLED", unset = "TRUE")
  if (!enabled)
    return(invisible(FALSE))

  renv_cleanse_sandbox()
  renv_cleanse_empty()

  invisible(TRUE)

}

renv_cleanse_sandbox <- function(sandbox = NULL) {

  # get sandbox root path
  sandbox <- sandbox %||% renv_paths_sandbox()
  root <- dirname(sandbox)
  if (!file.exists(sandbox))
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

renv_cleanse_empty <- function(path = NULL) {

  # no-op for Solaris
  if (renv_platform_solaris())
    return(FALSE)

  # move to path
  path <- path %||% renv_paths_root()
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
