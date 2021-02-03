
local({

  # the requested version of renv
  version <- "${VERSION}"

  # the project directory
  project <- getwd()

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

  # load bootstrap tools ${BOOTSTRAP}

  # load the renv profile, if any
  renv_bootstrap_profile_load(project)

  # construct path to library root
  root <- renv_bootstrap_library_root(project)

  # construct library prefix for platform
  prefix <- renv_bootstrap_prefix()

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
