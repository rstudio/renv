
local({

  # the requested version of renv
  version <- "${VERSION}"

  # the project directory
  project <- getwd()

  # figure out whether the autoloader is enabled
  enabled <- local({

    # first, check config option
    override <- getOption("renv.config.autoloader.enabled")
    if (!is.null(override))
      return(override)

    # next, check environment variables
    # TODO: prefer using the configuration one in the future
    envvars <- c(
      "RENV_CONFIG_AUTOLOADER_ENABLED",
      "RENV_AUTOLOADER_ENABLED",
      "RENV_ACTIVATE_PROJECT"
    )

    for (envvar in envvars) {
      envval <- Sys.getenv(envvar, unset = NA)
      if (!is.na(envval))
        return(tolower(envval) %in% c("true", "t", "1"))
    }

    # add empty line to break up bootstrapping from normal output
    catf("")

    # enable by default
    TRUE

  })

  if (!enabled)
    return(FALSE)

  # avoid recursion
  if (identical(getOption("renv.autoloader.running"), TRUE)) {
    warning("ignoring recursive attempt to run renv autoloader")
    return(invisible(TRUE))
  }

  # signal that we're loading renv during R startup
  options(renv.autoloader.running = TRUE)
  on.exit(options(renv.autoloader.running = NULL), add = TRUE)

  # signal that we've consented to use renv
  options(renv.consent = TRUE)

  # load the 'utils' package eagerly -- this ensures that renv shims, which
  # mask 'utils' packages, will come first on the search path
  library(utils, lib.loc = .Library)

  # unload renv if it's already been loaded
  if ("renv" %in% loadedNamespaces())
    unloadNamespace("renv")

  # load bootstrap tools ${BOOTSTRAP}

  # load the renv profile, if any
  renv_bootstrap_profile_load(project)

  # construct path to library root
  root <- renv_bootstrap_library_root(project)

  # construct library prefix for platform
  prefix <- renv_bootstrap_platform_prefix()

  # construct full libpath
  libpath <- file.path(root, prefix)

  # attempt to load
  if (renv_bootstrap_load(project, libpath, version))
    return(TRUE)

  if (renv_bootstrap_in_rstudio()) {
    setHook("rstudio.sessionInit", function(...) {
      renv_bootstrap_bootstrap(version, libpath)
    })
  } else {
    renv_bootstrap_bootstrap(version, libpath)
  }

  invisible()

})
