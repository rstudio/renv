
#' Load a Project
#'
#' Load an `renv` project.
#'
#' Calling `renv::load()` will set the session's library paths to use a
#' project-local library, and perform some other work to ensure the project is
#' properly isolated from other packages on the system.
#'
#' Normally, `renv::load()` is called automatically by the project auto-loader
#' written to the project `.Rprofile` by [renv::init()]. This allows \R sessions
#' launched from the root of an `renv` project directory to automatically load
#' that project, without requiring explicit action from the user. However, if
#' preferred or necessary, one can call `renv::load("<project>")` to explicitly
#' load an `renv` project located at a particular path.
#'
#' Use [renv::activate()] to activate (or re-activate) an `renv` project, so
#' that newly-launched \R sessions can automatically load the associated
#' project.
#'
#' @inherit renv-params
#'
#' @param quiet Boolean; be quiet during load?
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # load a project -- note that this is normally done automatically
#' # by the project's auto-loader, but calling this explicitly to
#' # load a particular project may be useful in some circumstances
#' renv::load()
#'
#' }
load <- function(project = NULL, quiet = FALSE) {

  renv_scope_error_handler()

  project <- project %||% renv_project_find(project)
  renv_scope_lock(project = project)

  # indicate that we're now loading the project
  renv_scope_options(renv.load.running = TRUE)

  # avoid suppressing the next auto snapshot
  renv_scope_var("running", TRUE, envir = `_renv_snapshot_auto`)

  # if load is being called via the autoloader,
  # then ensure RENV_PROJECT is unset
  # https://github.com/rstudio/renv/issues/887
  if (identical(getOption("renv.autoloader.running"), TRUE))
    renv_project_clear()

  # if we're loading a project different from the one currently loaded,
  # then unload the current project and reload the requested one
  switch <-
    !renv_metadata_embedded() &&
    !is.na(Sys.getenv("RENV_PROJECT", unset = NA)) &&
    !identical(project, renv_project())

  if (switch)
    return(renv_load_switch(project))

  if (quiet)
    renv_scope_options(renv.verbose = FALSE)

  renv_envvars_save()

  # load a minimal amount of state when testing
  if (renv_tests_running())
    return(renv_load_minimal(project))

  # load rest of renv components
  renv_load_init(project)
  renv_load_path(project)
  renv_load_shims(project)
  renv_load_renviron(project)
  renv_load_profile(project)
  renv_load_settings(project)
  renv_load_project(project)
  renv_load_sandbox(project)
  renv_load_libpaths(project)
  renv_load_rprofile(project)
  renv_load_cache(project)

  lockfile <- renv_lockfile_load(project)
  if (length(lockfile)) {
    renv_load_r(project, lockfile$R)
    renv_load_python(project, lockfile$Python)
    renv_load_bioconductor(project, lockfile$Bioconductor)
  }

  # allow failure to write infrastructure here to be non-fatal
  # https://github.com/rstudio/renv/issues/574#issuecomment-731159197
  catch({
    renv_infrastructure_write_rbuildignore(project)
    renv_infrastructure_write_gitignore(project)
  })

  renv_load_finish(project, lockfile)

  invisible(project)
}

renv_load_minimal <- function(project) {

  renv_load_libpaths(project)

  lockfile <- renv_lockfile_load(project)
  if (length(lockfile))
    renv_load_python(project, lockfile$Python)

  invisible(project)

}

renv_load_r <- function(project, fields) {

  # check for missing fields
  if (is.null(fields)) {
    warning("missing required [R] section in lockfile")
    return(NULL)
  }

  # load repositories
  renv_load_r_repos(fields$Repositories)

  # load (check) version
  version <- fields$Version
  if (is.null(version)) {
    warning("no R version recorded in this lockfile")
    return(NULL)
  }

  # normalize versions as plain old vectors
  requested <- unclass(numeric_version(version))[[1]]
  current <- unclass(numeric_version(getRversion()))[[1]]

  # only compare major, minor versions
  if (!identical(requested[1:2], current[1:2])) {
    fmt <- "Using R %s (lockfile was generated with R %s)"
    infof(fmt, getRversion(), version)
  }

}

renv_load_r_repos <- function(repos) {

  # force a character vector (https://github.com/rstudio/renv/issues/127)
  repos <- convert(repos, "character")

  # remove trailing slashes
  nms <- names(repos)
  repos <- sub("/+$", "", repos)
  names(repos) <- nms

  # convert to rspm if enabled
  if (renv_rspm_enabled())
    repos <- renv_rspm_transform(repos)

  # normalize option
  repos <- renv_repos_normalize(repos)

  # set sanitized repos
  options(repos = repos)

  # and return
  repos

}

renv_load_init <- function(project) {

  # warn if the project path cannot be translated into the native encoding,
  # as (especially on Windows) this will likely prevent renv from working
  actual <- enc2utf8(project)
  expected <- catch(enc2utf8(enc2native(actual)))
  if (identical(actual, expected))
    return(TRUE)

  msg <- paste(
    "the project path cannot be represented in the native encoding;",
    "renv may not function as expected"
  )

  warning(msg)

}

renv_load_path <- function(project) {

  # only required when running in RStudio
  if (!renv_rstudio_available())
    return(FALSE)

  # on macOS, read paths from /etc/paths and friends

  # nocov start
  if (renv_platform_macos()) {

    # read the current PATH
    old <- renv_envvar_get("PATH", unset = "") %>%
      strsplit(split = .Platform$path.sep, fixed = TRUE) %>%
      unlist()

    # get the new PATH entries
    files <- c(
      if (file.exists("/etc/paths")) "/etc/paths",
      list.files("/etc/paths.d", full.names = TRUE)
    )

    new <- uapply(files, readLines, warn = FALSE)

    # mix them together, preferring things in /etc/paths
    mix <- unique(c(new, old))

    # update the PATH
    Sys.setenv(PATH = paste(mix, collapse = .Platform$path.sep))

  }
  # nocov end

}

renv_load_shims <- function(project) {
  if (renv_shims_enabled())
    renv_shims_activate()
}

renv_load_renviron <- function(project) {

  environs <- c(
    renv_paths_root(".Renviron"),
    if (config$user.environ())
      Sys.getenv("R_ENVIRON_USER", unset = "~/.Renviron"),
    file.path(project, ".Renviron")
  )

  for (environ in environs)
    if (file.exists(environ))
      readRenviron(environ)

  renv_envvars_normalize()

}

renv_load_profile <- function(project) {

  renv_bootstrap_profile_load(project = project)

}

renv_load_settings <- function(project) {

  settings <- renv_paths_renv("settings.R", project = project)
  if (!file.exists(settings))
    return(FALSE)

  tryCatch(
    eval(parse(settings), envir = baseenv()),
    error = warning
  )

  TRUE

}

renv_load_project <- function(project) {

  # record the active project in this session
  project <- renv_path_normalize(project, winslash = "/")
  Sys.setenv(RENV_PROJECT = project)

  # update project list if enabled
  enabled <- renv_cache_config_enabled(project = project)
  if (enabled)
    renv_load_project_projlist(project)

  TRUE

}

renv_load_project_projlist <- function(project) {

  # read project list
  projects <- renv_paths_root("projects")
  projlist <- character()
  if (file.exists(projects))
    projlist <- readLines(projects, warn = FALSE, encoding = "UTF-8")

  # if the project is already recorded, nothing to do
  if (project %in% projlist)
    return(TRUE)

  # sort with C locale (ensure consistent sorting across OSes)
  projlist <- local({
    renv_scope_locale("LC_COLLATE", "C")
    sort(c(projlist, project))
  })

  # update the project list
  ensure_parent_directory(projects)
  catchall(writeLines(enc2utf8(projlist), projects, useBytes = TRUE))

  TRUE

}

renv_load_rprofile <- function(project = NULL) {

  project <- renv_project_resolve(project)

  # bail if not enabled by user
  enabled <- identical(config$user.profile(), TRUE)
  if (!enabled)
    return(FALSE)

  # callr will manage sourcing of user profile, so don't try
  # to source the user profile if we're in callr
  callr <- Sys.getenv("CALLR_CHILD_R_LIBS", unset = NA)
  if (!is.na(callr))
    return(FALSE)

  # check for existence of profile
  profile <- Sys.getenv("R_PROFILE_USER", unset = "~/.Rprofile")
  if (!file.exists(profile))
    return(FALSE)

  renv_scope_libpaths()
  renv_load_rprofile_impl(profile)

  TRUE

}

renv_load_rprofile_impl <- function(profile) {

  # NOTE: We'd like to use a regular tryCatch() handler here, but
  # that will cause issues for user profiles which attempt to add
  # global calling handlers. For that reason, we just register a
  # bare restart handler, so at least we can catch the jump.
  #
  # https://github.com/rstudio/renv/issues/1036

  status <- withRestarts(
    eval(parse(profile), envir = globalenv()),
    abort = function() { structure(list(), class = "_renv_error") }
  )

  if (inherits(status, "_renv_error")) {
    fmt <- "an error occurred while sourcing %s"
    warningf(fmt, renv_path_pretty(profile))
  }

  FALSE

}

renv_load_libpaths <- function(project = NULL) {
  renv_libpaths_activate(project)
  libpaths <- renv_libpaths_all()
  lapply(libpaths, renv_library_diagnose, project = project)
  Sys.setenv(R_LIBS_USER = paste(libpaths, collapse = .Platform$path.sep))
}

renv_load_sandbox <- function(project) {
  renv_sandbox_activate(project)
}

renv_load_python <- function(project, fields) {

  python <- tryCatch(
    renv_load_python_impl(project, fields),
    error = function(e) {
      warning(e)
      NULL
    }
  )

  if (is.null(python))
    return(FALSE)

  # set environment variables
  # - RENV_PYTHON is the version of Python renv was configured to use
  # - RETICULATE_PYTHON used to configure version of Python used by reticulate
  Sys.setenv(
    RENV_PYTHON       = python,
    RETICULATE_PYTHON = python
  )

  # place python + relevant utilities on the PATH
  bindir <- normalizePath(dirname(python), mustWork = FALSE)
  renv_envvar_prepend("PATH", bindir)

  # on Windows, for conda environments, we may also have a Scripts directory
  # which will need to be pre-pended to the PATH
  if (renv_platform_windows()) {
    scriptsdir <- file.path(bindir, "Scripts")
    if (file.exists(scriptsdir))
      renv_envvar_prepend("PATH", scriptsdir)
  }

  # for conda environments, we should try to find conda and place the conda
  # executable on the PATH, in case users want to use conda e.g. from
  # the terminal or even via R system calls
  #
  # we'll also need to set some environment variables to ensure that conda
  # uses this environment by default
  info <- renv_python_info(python)
  if (identical(info$type, "conda")) {
    conda <- renv_conda_find(python)
    if (file.exists(conda)) {
      renv_envvar_prepend("PATH", dirname(conda))
      Sys.setenv(CONDA_PREFIX = info$root)
    }
  }

  TRUE

}

renv_load_python_impl <- function(project, fields) {

  # if RENV_PYTHON is already set, just use it
  python <- Sys.getenv("RENV_PYTHON", unset = NA)
  if (!is.na(python))
    return(python)

  # set a default reticulate Python environment path
  envpath <- renv_paths_renv("python/r-reticulate", project = project)
  Sys.setenv(RETICULATE_MINICONDA_PYTHON_ENVPATH = envpath)

  # nothing more to do if no lockfile fields set
  if (is.null(fields))
    return(NULL)

  # delegate based on type appropriately
  type <- fields$Type
  if (is.null(type))
    return(NULL)

  python <- switch(type,
    system     = renv_load_python_default(project, fields),
    virtualenv = renv_load_python_virtualenv(project, fields),
    conda      = renv_load_python_condaenv(project, fields),
    stopf("unrecognized Python type '%s'", type)
  )

  renv_path_canonicalize(python)

}

renv_load_python_default <- function(project, fields) {

  # if 'Name' points to a valid copy of Python, use it
  name <- fields$Name
  if (!is.null(name) && file.exists(name))
    return(name)

  # otherwise, try to find a compatible version of Python
  renv_python_find(fields$Version)

}

renv_load_python_virtualenv <- function(project, fields) {

  renv_use_python_virtualenv_impl(
    project = project,
    name    = fields[["Name"]]    %NA% NULL,
    version = fields[["Version"]] %NA% NULL,
    python  = fields[["Python"]]  %NA% NULL
  )

}

renv_load_python_condaenv <- function(project, fields) {

  renv_use_python_condaenv_impl(
    project = project,
    name    = fields[["Name"]]    %NA% NULL,
    version = fields[["Version"]] %NA% NULL,
    python  = fields[["Python"]]  %NA% NULL
  )

}

renv_load_bioconductor <- function(project, bioconductor) {

  # we don't try to support older R anymore
  if (getRversion() < "3.4")
    return()

  # if we don't have a valid Bioconductor version, bail
  version <- bioconductor$Version
  if (is.null(version))
    return()

  # initialize bioconductor
  renv_bioconductor_init()

  # validate version if necessary
  validate <- getOption("renv.bioconductor.validate")
  if (truthy(validate, default = TRUE))
    renv_load_bioconductor_validate(project, version)

  # update the R repositories
  repos <- renv_bioconductor_repos(project, version)
  options(repos = repos)

  # notify the user
  sprintf("* Using Bioconductor '%s'.", version)

}

renv_load_bioconductor_validate <- function(project, version) {

  BiocManager <- renv_namespace_load("BiocManager")
  if (!is.function(BiocManager$.version_validity))
    return()

  # check for valid version of Bioconductor
  # https://github.com/rstudio/renv/issues/1148
  status <- catch(BiocManager$.version_validity(version))
  if (!is.character(status))
    return()

  fmt <- lines(
    "This project is configured to use Bioconductor %1$s, which is not compatible with R %2$s.",
    "Use 'renv::init(bioconductor = \"%1$s\")' to re-initialize this project with the appropriate Bioconductor release.",
    if (renv_package_installed("BiocVersion"))
      "Please uninstall the 'BiocVersion' package first, with `remove.packages(\"BiocVersion\")`."
  )

  warningf(fmt, version, getRversion())

}

renv_load_switch <- function(project) {

  # safety check: avoid recursive unload attempts
  unloading <- getOption("renv.unload.project")
  if (!is.null(unloading)) {
    fmt <- "ignoring recursive attempt to load project '%s'"
    warningf(fmt, renv_path_pretty(project))
    return(project)
  }

  # unset the RENV_PATHS_RENV environment variable
  # TODO: is there a path forward if different projects use
  # different RENV_PATHS_RENV paths?
  renvpath <- renv_envvar_get("RENV_PATHS_RENV")
  renv_envvar_clear("RENV_PATHS_RENV")

  # validate that this project has an activate script
  script <- renv_paths_activate(project = project)
  if (!file.exists(script)) {
    fmt <- "project %s has no activate script and so cannot be activated"
    stopf(fmt, renv_path_pretty(project))
  }

  # signal that we're unloading now
  renv_scope_options(renv.unload.project = project)

  # perform the unload
  unload()

  # unload the current version of renv (but keep track of position
  # on search path in case we need to revert later)
  path <- renv_namespace_path("renv")
  pos <- match("package:renv", search())
  unloadNamespace("renv")

  # move to new project directory
  owd <- setwd(project)
  on.exit(setwd(owd), add = TRUE)

  # source the activate script
  source(script)

  # check and see if renv was successfully loaded
  if (!"renv" %in% loadedNamespaces()) {
    fmt <- "could not load renv from project %s; reloading previously-loaded renv"
    warningf(fmt, renv_path_pretty(project))
    loadNamespace("renv", lib.loc = dirname(path))
    renv_envvar_set("RENV_PATHS_RENV", renvpath)
    if (!is.na(pos)) {
      args <- list(package = "renv", pos = pos, character.only = TRUE)
      do.call(base::library, args)
    }
  }

}

renv_load_cache <- function(project) {

  if (!interactive())
    return(FALSE)

  oldcache <- renv_paths_cache(version = renv_cache_version_previous())[[1L]]
  newcache <- renv_paths_cache(version = renv_cache_version())[[1L]]
  if (!file.exists(oldcache) || file.exists(newcache))
    return(FALSE)

  msg <- lines(
    "* The cache version has been updated in this version of renv.",
    "* Use `renv::rehash()` to migrate packages from the old renv cache."
  )

  vmessagef(msg)

}

renv_load_check <- function(project) {
  renv_load_check_description(project)
}

renv_load_check_description <- function(project) {

  descpath <- file.path(project, "DESCRIPTION")
  if (!file.exists(descpath))
    return(TRUE)

  contents <- readLines(descpath, warn = FALSE)
  bad <- which(grepl("^\\s*$", contents, perl = TRUE))
  if (empty(bad))
    return(TRUE)

  values <- sprintf("[line %i is blank]", bad)

  renv_pretty_print(
    values    = values,
    preamble  = sprintf(
      "%s contains blank lines:",
      renv_path_pretty(descpath)
    ),
    postamble = c(
      "DESCRIPTION files cannot contain blank lines between fields.",
      "Please remove these blank lines from the file."
    ),
    wrap = FALSE
  )

  return(FALSE)

}

renv_load_quiet <- function() {
  default <- identical(renv_verbose(), FALSE) || renv_session_quiet()
  config$startup.quiet(default = default)
}

renv_load_finish <- function(project, lockfile) {

  options(renv.project.path = project)

  renv_load_check(project)

  if (!renv_load_quiet()) {
    renv_load_report_project(project)
    renv_load_report_python(project)
  }

  renv_load_report_updates(project)
  renv_load_report_synchronized(project, lockfile)

  renv_snapshot_auto_update(project = project)

}

renv_load_report_project <- function(project) {

  profile <- renv_profile_get()
  version <- renv_metadata_version()

  if (length(profile)) {
    fmt <- "* (%s) Project '%s' loaded. [renv %s]"
    vwritef(fmt, profile, renv_path_aliased(project), version)
  } else {
    fmt <- "* Project '%s' loaded. [renv %s]"
    vwritef(fmt, renv_path_aliased(project), version)
  }

}

renv_load_report_python <- function(project) {

  python <- Sys.getenv("RENV_PYTHON", unset = NA)
  if (is.na(python))
    return(FALSE)

  # fmt <- "* Using Python %s. [%s]"
  # vwritef(fmt, renv_python_version(python), renv_python_type(python))

}

renv_load_report_updates <- function(project) {

  # nocov start
  enabled <- interactive() && config$updates.check()
  if (!enabled)
    return(FALSE)

  callback <- function(...) renv_load_report_updates_impl(project = project)
  renv_load_invoke(callback)
  # nocov end

}

# nocov start
renv_load_report_updates_impl <- function(project) {

  lockpath <- renv_lockfile_path(project = project)
  if (!file.exists(lockpath))
    return(FALSE)

  status <- update(project = project, check = TRUE)
  available <- inherits(status, "renv_updates") && length(status$diff)
  if (!available)
    return(FALSE)

  vwritef("* Use `renv::update()` to install updated packages.")
  if (!interactive())
    print(status)

  TRUE

}
# nocov end

renv_load_report_synchronized <- function(project, lockfile) {

  # nocov start
  enabled <- interactive() && config$synchronized.check()
  if (!enabled)
    return(FALSE)

  # TODO: This can be slow. I wonder if there's a sensible way to farm this
  # out to a separate R process, and then collect the results later on?
  #
  # Just using 'system(..., wait = FALSE)' feels a bit awkward, as we might
  # end up emitting output while the user is typing in the terminal, which
  # feels disruptive.
  callback <- function(...) renv_project_synchronized_check(project, lockfile)
  renv_load_invoke(callback)
  # nocov end

}

renv_load_invoke <- function(callback) {

  # helper function for running code that might need to
  # wait until RStudio has finished initializing
  if (renv_rstudio_loading())
    setHook("rstudio.sessionInit", callback, action = "append")
  else
    callback()

}

