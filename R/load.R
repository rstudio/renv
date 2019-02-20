
# tools for loading an renv (typically done at R session init)
renv_load_r_version <- function(version) {
  if (version_compare(version, getRversion()) != 0) {
    fmt <- "Environment '%s' requested R version '%s' but '%s' is currently being used"
    warningf(fmt, renv_state$environment(), version, getRversion())
  }
}

renv_load_libpaths <- function(project = NULL) {
  project <- project %||% renv_state$project()
  libpaths <- renv_paths_library(project)
  Sys.setenv(R_LIBS_USER = paste(libpaths, collapse = .Platform$path.sep))
  .libPaths(libpaths)
}

renv_load_repos <- function(repos) {
  options(repos = repos)
}

renv_load_envvars <- function(project = NULL) {
  project <- project %||% renv_state$project()
  Sys.setenv(
    R_PROFILE_USER = "",
    R_ENVIRON_USER = file.path(project, ".Renviron")
  )
}

renv_load_python <- function(manifest) {

  # get path to Python
  python <- manifest$Python$Path
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

  renv_state$python(python)
  return(TRUE)

}

renv_load_finish <- function() {
  # TODO: report to user?
}
