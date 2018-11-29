renv_active_state <- function(name, override, default) {
  if (!is.null(override))
    return(override)
  else
    Sys.getenv(name, unset = default)
}

renv_active_project <- function(project) {
  renv_active_state("RENV_ACTIVE_PROJECT", project, getwd())
}

renv_set_active_project <- function(project) {
  Sys.setenv("RENV_ACTIVE_PROJECT" = normalizePath(project, winslash = "/"))
}

renv_active_renv <- function(renv) {
  renv_active_state("RENV_ACTIVE_RENV", renv, NULL)
}

renv_set_active_renv <- function(renv) {
  Sys.setenv("RENV_ACTIVE_RENV" = renv)
}

