
local({

  # read project state
  activate <- "renv/activate.dcf"
  if (!file.exists(activate))
    return()

  # attempt to determine requested renv version
  version <- tryCatch(read.dcf(activate, fields = "Version"), error = identity)
  if (inherits(version, "error"))
    return(warning(version))

  # try to find a path where renv might be installed
  prefix <- file.path(R.version$platform, getRversion()[1, 1:2])
  base <- c(Sys.getenv("RENV_PATHS_ROOT", unset = "~/.renv"), "renv")
  paths <- file.path(base, "bootstrap", prefix, "renv", version)

  # try to load renv from one of these paths
  for (path in paths) {
    if (requireNamespace("renv", lib.loc = path, quietly = TRUE)) {
      renv::load()
      return(invisible())
    }
  }

  # failed to load renv; warn the user
  msg <- "Failed to find an renv installation: the project will not be loaded."
  warning(msg, call. = FALSE)

})
