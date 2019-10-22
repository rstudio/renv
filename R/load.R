
# tools for loading an renv (typically done at R session init)
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
  if (is.null(version))
    return(NULL)

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

  # set sanitized repos
  options(repos = repos)

  # and return
  repos

}

renv_load_path <- function(project) {

  # only required when running in RStudio
  if (identical(.Platform$GUI, "RStudio"))
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

}

renv_load_settings <- function(project) {

  settings <- file.path(project, "renv/settings.R")
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
  writeLines(enc2utf8(projlist), projects, useBytes = TRUE)

  TRUE

}

renv_load_profile <- function(project = NULL) {

  project <- project %||% renv_project()

  enabled <- renv_config("user.profile", default = TRUE)
  if (!enabled)
    return(FALSE)

  renv_scope_libpaths()

  profile <- Sys.getenv("R_PROFILE_USER", unset = "~/.Rprofile")
  if (file.exists(profile))
    renv_load_profile_impl(profile)

  TRUE

}

renv_load_profile_impl <- function(profile) {

  status <- catch(eval(parse(profile), envir = globalenv()))
  if (inherits(status, "error")) {
    fmt <- paste("Error sourcing '%s': %s")
    text <- sprintf(fmt, aliased_path(profile), conditionMessage(status))
    all <- c(text, "", status$traceback)
    writeLines(all, con = stderr())
    return(FALSE)
  }

  TRUE
}

renv_load_libpaths <- function(project = NULL) {
  renv_libpaths_save()
  renv_libpaths_activate(project)
  libpaths <- renv_libpaths_all()
  lapply(libpaths, renv_library_diagnose, project = project)
  Sys.setenv(R_LIBS_USER = paste(libpaths, collapse = .Platform$path.sep))
}

renv_load_sandbox <- function(project) {
  if (renv_sandbox_enabled(project))
    renv_sandbox_activate(project)
}

renv_load_updates <- function(project) {

  if (!is.na(Sys.getenv("RSTUDIO", unset = NA))) {
    update <- function() renv_load_updates_impl(project = project)
    setHook("rstudio.sessionInit", update)
    return(TRUE)
  }

  renv_load_updates_impl(project = project)

}

renv_load_updates_impl <- function(project) {

  if (!renv_config("updates.check", default = FALSE))
    return(FALSE)

  if (!file.exists(file.path(project, "renv.lock")))
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

renv_load_python <- function(project, fields) {

  # set a default reticulate Python environment path
  envpath <- file.path(project, "renv/python/r-reticulate")
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

renv_load_finish <- function(project) {

  quiet <-
    "--slave" %in% commandArgs(trailingOnly = FALSE) ||
    identical(renv_verbose(), FALSE)

  if (!quiet) {
    fmt <- "* Project '%s' loaded. [renv %s]"
    vwritef(fmt, aliased_path(project), renv_package_version("renv"))
  }

  renv_load_updates(project)

}
