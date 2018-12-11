
renv_active_state <- function(name, override, default) {
  if (!is.null(override))
    override
  else
    Sys.getenv(name, unset = default)
}

renv_active_project <- function(project = NULL) {
  state <- renv_active_state("RENV_ACTIVE_PROJECT", project, getwd())
  as.character(state)
}

renv_set_active_project <- function(project) {
  Sys.setenv(RENV_ACTIVE_PROJECT = normalizePath(project, winslash = "/"))
}

renv_active_renv <- function(renv = NULL) {
  state <- renv_active_state("RENV_ACTIVE_RENV", renv, "")
  as.character(state)
}

renv_set_active_renv <- function(renv) {
  Sys.setenv(RENV_ACTIVE_RENV = renv)
}

renv_local <- function(local = NULL) {
  state <- renv_active_state("RENV_LOCAL", local, FALSE)
  as.logical(state)
}

renv_set_local <- function(local) {
  Sys.setenv(RENV_LOCAL = local)
}

renv_active_manifest <- function(project = NULL) {
  file.path(renv_active_project(project), "renv/manifest")
}
