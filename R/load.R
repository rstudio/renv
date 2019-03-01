
# tools for loading an renv (typically done at R session init)
renv_load_r_version <- function(version) {
  if (version_compare(version, getRversion()) != 0) {
    fmt <- "Project requested R version '%s' but '%s' is currently being used"
    warningf(fmt, version, getRversion())
  }
}

renv_load_project <- function(project) {
  Sys.setenv(RENV_PROJECT = normalizePath(project, winslash = "/"))
}

renv_load_profile <- function(project = NULL) {
  project <- project %||% renv_project()

  profile <- renv_paths_root(".Rprofile")
  if (!file.exists(profile))
    return(FALSE)

  status <- catch(source(profile))
  if (inherits(status, "error")) {
    fmt <- paste("Error sourcing '%s': %s")
    warningf(fmt, aliased_path(profile), conditionMessage(status))
    return(FALSE)
  }

  TRUE
}

renv_load_envvars <- function(project = NULL) {
  project <- project %||% renv_project()
  Sys.setenv(
    R_PROFILE_USER = "",
    R_ENVIRON_USER = file.path(project, ".Renviron")
  )
}

renv_load_libpaths <- function(project = NULL) {
  project <- project %||% renv_project()
  libpaths <- c(renv_paths_library(project), settings$external.libraries())
  lapply(libpaths, ensure_directory)
  lapply(libpaths, renv_library_diagnose, project = project)
  Sys.setenv(R_LIBS_USER = paste(libpaths, collapse = .Platform$path.sep))
  .libPaths(libpaths)
}

renv_load_repos <- function(repos) {
  options(repos = repos)
}

renv_load_python <- function(lockfile) {

  # get path to Python
  python <- lockfile$Python$Path
  if (is.null(python))
    return(FALSE)

  if (!requireNamespace("reticulate", quietly = TRUE))
    install("reticulate")

  # resolve path to Python binary (if this was e.g. a virtualenv)
  python <- renv_python_resolve(python)

  # check to see if we've been given the path to a Python executable,
  # or a directory containing a virtual environment.
  # TODO: what if no virtual environment exists? should auto-create it?
  # TODO: what about auto-creating / auto-using project-local virtualenvs?
  info <- file.info(python, extra_cols = FALSE)

  if (is.na(info$isdir)) {
    fmt <- "There is no Python installation at path '%s'"
    warningf(fmt, python)
    return(FALSE)
  }

  status <- catch(case(
    identical(info$isdir, TRUE)  ~ reticulate::use_virtualenv(python, required = TRUE),
    identical(info$isdir, FALSE) ~ reticulate::use_python(python, required = TRUE)
  ))

  if (inherits(status, "error")) {
    warning(status)
    return(FALSE)
  }

  return(TRUE)

}

renv_load_finish <- function() {
  # TODO: report to user?
}
