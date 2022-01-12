
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

  # load failed; inform user we're about to bootstrap
  prefix <- paste("# Bootstrapping renv", version)
  postfix <- paste(rep.int("-", 77L - nchar(prefix)), collapse = "")
  header <- paste(prefix, postfix)
  message(header)

  # perform bootstrap
  bootstrap(version, libpath)

  # exit early if we're just testing bootstrap
  if (!is.na(Sys.getenv("RENV_BOOTSTRAP_INSTALL_ONLY", unset = NA)))
    return(TRUE)

  # try again to load
  if (requireNamespace("renv", lib.loc = libpath, quietly = TRUE)) {
    message("* Successfully installed and loaded renv ", version, ".")
    return(renv::load())
  }

  # failed to download or load renv; warn the user
  msg <- c(
    "Failed to find an renv installation: the project will not be loaded.",
    "Use `renv::activate()` to re-initialize the project."
  )

  warning(paste(msg, collapse = "\n"), call. = FALSE)

})
