
local({

  # the requested version of renv
  version <- "${VERSION}"

  # avoid recursion
  if (!is.na(Sys.getenv("RENV_R_INITIALIZING", unset = NA)))
    return(invisible(TRUE))

  # signal that we're loading renv during R startup
  Sys.setenv("RENV_R_INITIALIZING" = "true")
  on.exit(Sys.unsetenv("RENV_R_INITIALIZING"), add = TRUE)

  # signal that we've consented to use renv
  options(renv.consent = TRUE)

  # load the 'utils' package eagerly -- this ensures that renv shims, which
  # mask 'utils' packages, will come first on the search path
  library(utils, lib.loc = .Library)

  # check to see if renv has already been loaded
  if ("renv" %in% loadedNamespaces()) {

    # if renv has already been loaded, and it's the requested version of renv,
    # nothing to do
    spec <- .getNamespaceInfo(.getNamespace("renv"), "spec")
    if (identical(spec[["version"]], version))
      return(invisible(TRUE))

    # otherwise, unload and attempt to load the correct version of renv
    unloadNamespace("renv")

  }

  # construct path to renv in library
  libpath <- local({

    root <- Sys.getenv("RENV_PATHS_LIBRARY", unset = "renv/library")
    prefix <- paste("R", getRversion()[1, 1:2], sep = "-")

    # include SVN revision for development versions of R
    # (to avoid sharing platform-specific artefacts with released versions of R)
    devel <-
      identical(R.version[["status"]],   "Under development (unstable)") ||
      identical(R.version[["nickname"]], "Unsuffered Consequences")

    if (devel)
      prefix <- paste(prefix, R.version[["svn rev"]], sep = "-r")

    file.path(root, prefix, R.version$platform)

  })

  # try to load renv from the project library
  if (requireNamespace("renv", lib.loc = libpath, quietly = TRUE)) {

    # warn if the version of renv loaded does not match
    loadedversion <- utils::packageDescription("renv", fields = "Version")
    if (version != loadedversion) {

      # assume four-component versions are from GitHub; three-component
      # versions are from CRAN
      components <- strsplit(loadedversion, "[.-]")[[1]]
      remote <- if (length(components) == 4L)
        paste("rstudio/renv", loadedversion, sep = "@")
      else
        paste("renv", loadedversion, sep = "@")

      fmt <- paste(
        "renv %1$s was loaded from project library, but renv %2$s is recorded in lockfile.",
        "Use `renv::record(\"%3$s\")` to record this version in the lockfile.",
        "Use `renv::restore(packages = \"renv\")` to install renv %2$s into the project library.",
        sep = "\n"
      )

      msg <- sprintf(fmt, loadedversion, version, remote)
      warning(msg, call. = FALSE)

    }

    # load the project
    return(renv::load())

  }

  # try to bootstrap an renv installation ${BOOTSTRAP}
  bootstrap(version, libpath)

  # try again to load
  if (requireNamespace("renv", lib.loc = libpath, quietly = TRUE)) {
    message("Successfully installed and loaded renv ", version, ".")
    return(renv::load())
  }

  # failed to download or load renv; warn the user
  msg <- c(
    "Failed to find an renv installation: the project will not be loaded.",
    "Use `renv::activate()` to re-initialize the project."
  )

  warning(paste(msg, collapse = "\n"), call. = FALSE)

})
