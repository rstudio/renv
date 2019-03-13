
local({

  # requested version of renv
  version <- "%s"

  # try to find a path where renv might be installed
  prefix <- file.path(R.version$platform, getRversion()[1, 1:2])
  base <- c(Sys.getenv("RENV_PATHS_ROOT", unset = "~/.renv"), "renv")
  paths <- file.path(base, "bootstrap", prefix, "renv", version)

  # try to load renv from one of these paths
  for (path in paths)
    if (requireNamespace("renv", lib.loc = path, quietly = TRUE))
      return(renv::load())

  # failed to load renv; warn the user
  msg <- c(
    "Failed to find an renv installation: the project will not be loaded.",
    "Use `renv::activate()` to re-initialize the project."
  )

  warning(paste(msg, collapse = "\n"), call. = FALSE)

})
