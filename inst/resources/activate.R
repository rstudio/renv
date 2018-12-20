local({

  # read project state
  version <- renv_dcf_read("renv/renv.dcf", fields = "Version")

  # try to find a path where 'renv' might be installed
  prefix <- file.path(R.version$platform, getRversion()[1, 1:2])
  base <- c("renv", Sys.getenv("RENV_PATHS_ROOT", unset = "~/.renv"))
  paths <- file.path(base, "bootstrap", prefix, "renv", version)

  # try to load renv from one of these paths
  for (path in paths) {
    if (requireNamespace("renv", lib.loc = path, quietly = TRUE)) {
      renv::load()
      return(invisible())
    }
  }

  # failed to load renv; warn the user
  msg <- paste(
    "Unable to find a local renv installation;",
    "the active virtual environment will not be loaded."
  )

  warning(msg, call. = FALSE)

})
