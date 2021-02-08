
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
load <- function(project = getwd(), quiet = FALSE) {

  renv_scope_error_handler()
  renv_scope_lock(project = project)

  # if we're loading a project different from the one currently loaded,
  # then unload the current project and reload the requested one
  switch <-
    !is.na(Sys.getenv("RENV_PROJECT", unset = NA)) &&
    !identical(project, renv_project())

  if (switch)
    return(renv_load_switch(project))

  if (quiet)
    renv_scope_options(renv.verbose = FALSE)

  renv_envvars_save()

  # load a minimal amount of state when testing
  if (renv_tests_running()) {
    renv_load_libpaths(project)
    return(invisible(project))
  }

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
    fmt <- "Project requested R version '%s' but '%s' is currently being used"
    warningf(fmt, version, getRversion())
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

    files <- c(
      "/etc/paths",
      list.files("/etc/paths.d", full.names = TRUE)
    )

    PATH <- unique(uapply(files, readLines, warn = FALSE))
    Sys.setenv(PATH = paste(PATH, collapse = .Platform$path.sep))
    return(TRUE)

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
    Sys.getenv("R_ENVIRON_USER", unset = "~/.Renviron"),
    file.path(project, ".Renviron")
  )

  for (environ in environs)
    if (file.exists(environ))
      readRenviron(environ)

  Sys.setenv(R_LIBS_SITE = .expand_R_libs_env_var(Sys.getenv("R_LIBS_SITE")))
  Sys.setenv(R_LIBS_USER = .expand_R_libs_env_var(Sys.getenv("R_LIBS_USER")))

}

renv_load_profile <- function(project) {

  renv_bootstrap_profile_load(project = project)

}

renv_load_settings <- function(project) {

  components <- c(project, renv_profile_prefix(), "renv/settings.R")
  settings <- paste(components, collapse = "/")
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

  # read project list
  projects <- renv_paths_root("projects")
  projlist <- character()
  if (file.exists(projects))
    projlist <- readLines(projects, warn = FALSE, encoding = "UTF-8")

  # if the project is already recorded, nothing to do
  if (project %in% projlist)
    return(TRUE)

  # otherwise, update the project list
  renv_scope_locale("LC_COLLATE", "C")
  projlist <- sort(c(projlist, project))
  ensure_parent_directory(projects)
  catchall(writeLines(enc2utf8(projlist), projects, useBytes = TRUE))

  TRUE

}

renv_load_rprofile <- function(project = NULL) {

  project <- renv_project_resolve(project)

  enabled <- config$user.profile()
  if (!enabled)
    return(FALSE)

  renv_scope_libpaths()

  profile <- Sys.getenv("R_PROFILE_USER", unset = "~/.Rprofile")
  if (file.exists(profile))
    renv_load_rprofile_impl(profile)

  TRUE

}

renv_load_rprofile_impl <- function(profile) {

  status <- catch(eval(parse(profile), envir = globalenv()))
  if (!inherits(status, "error"))
    return(TRUE)

  fmt <- "error sourcing %s: %s"
  warningf(fmt, renv_path_pretty(profile), conditionMessage(status))
  if (!renv_tests_running())
    writeLines(status$traceback, con = stderr())

  FALSE

}

renv_load_libpaths <- function(project = NULL) {
  renv_libpaths_activate(project)
  libpaths <- renv_libpaths_all()
  lapply(libpaths, renv_library_diagnose, project = project)
  Sys.setenv(R_LIBS_USER = paste(libpaths, collapse = .Platform$path.sep))
}

renv_load_sandbox <- function(project) {
  if (config$sandbox.enabled())
    renv_sandbox_activate(project)
}

renv_load_python <- function(project, fields) {

  # set a default reticulate Python environment path
  components <- c(project, renv_profile_prefix(), "renv/python/r-reticulate")
  envpath <- paste(components, collapse = "/")
  Sys.setenv(RETICULATE_MINICONDA_PYTHON_ENVPATH = envpath)

  # nothing more to do if no lockfile fields set
  if (is.null(fields))
    return(FALSE)

  # delegate based on type appropriately
  type <- fields$Type
  if (is.null(type))
    return(FALSE)

  python <- switch(type,
    system     = renv_load_python_default(fields),
    virtualenv = renv_load_python_env(fields, renv_use_python_virtualenv),
    conda      = renv_load_python_env(fields, renv_use_python_condaenv),
    stopf("unrecognized Python type '%s'", type)
  )

  if (is.null(python))
    return(FALSE)

  Sys.setenv(RENV_PYTHON = python, RETICULATE_PYTHON = python)

  if (type %in% c("virtualenv", "conda")) {
    info <- renv_python_info(python)
    Sys.setenv(RETICULATE_PYTHON_ENV = info$root)
  }

  TRUE

}

renv_load_python_default <- function(fields) {
  renv_python_find(fields$Version)
}

renv_load_python_virtualenv <- function(fields) {
  renv_load_python_env(fields, renv_use_python_virtualenv)
}

renv_load_python_conda <- function(fields) {
  renv_load_python_env(fields, renv_use_python_condaenv)
}

renv_load_python_env <- function(fields, loader) {
  project <- renv_project()
  version <- fields$Version
  name    <- fields$Name %NA% NULL
  loader(project = project, version = version, name = name)
}

renv_load_switch <- function(project) {

  # safety check: avoid recursive unload attempts
  unloading <- getOption("renv.unloading")
  if (identical(unloading, TRUE)) {
    fmt <- "ignoring recursive attempt to load project '%s'"
    warningf(fmt, renv_path_pretty(project))
    return(project)
  }

  # validate that this project has an activate script
  script <- file.path(project, "renv/activate.R")
  if (!file.exists(script)) {
    fmt <- "project %s has no activate script and so cannot be activated"
    stopf(fmt, renv_path_pretty(project))
  }

  # signal that we're unloading now
  renv_scope_options(renv.unloading = TRUE)

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
  source("renv/activate.R")

  # check and see if renv was successfully loaded
  if (!"renv" %in% loadedNamespaces()) {
    fmt <- "could not load renv from project %s; reloading previously-loaded renv"
    warningf(fmt, renv_path_pretty(project))
    loadNamespace("renv", lib.loc = dirname(path))
    if (!is.na(pos)) {
      args <- list(package = "renv", pos = pos, character.only = TRUE)
      do.call(base::library, args)
    }
  }

}

renv_load_cache <- function(project) {

  if (!interactive())
    return(FALSE)

  oldcache <- renv_paths_cache(version = renv_cache_version_previous())
  newcache <- renv_paths_cache(version = renv_cache_version())
  if (!file.exists(oldcache) || file.exists(newcache))
    return(FALSE)

  msg <- lines(
    "* The cache version has been updated in this version of renv.",
    "* Use `renv::rehash()` to migrate packages from the old renv cache."
  )

  vmessagef(msg)

}

renv_load_finish <- function(project, lockfile) {

  renv_load_report_project(project)
  renv_load_report_updates(project)
  renv_load_report_synchronized(project, lockfile)
  renv_snapshot_auto_update(project = project)

}

renv_load_report_project <- function(project) {

  quiet <- config$startup.quiet() %||% (
    identical(renv_verbose(), FALSE) ||
    renv_session_quiet()
  )

  if (quiet)
    return()

  profile <- renv_profile_get()
  version <- renv_package_version("renv")

  if (length(profile)) {
    fmt <- "* (%s) Project '%s' loaded. [renv %s]"
    vwritef(fmt, profile, aliased_path(project), version)
  } else {
    fmt <- "* Project '%s' loaded. [renv %s]"
    vwritef(fmt, aliased_path(project), renv_package_version("renv"))
  }

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

