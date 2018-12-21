
`_renv_state` <- new.env(parent = emptyenv())

renv_state_get <- function(name, default) {
  if (exists(name, envir = `_renv_state`, inherits = FALSE))
    get(name, envir = `_renv_state`, inherits = FALSE)
  else
    default
}

renv_state_set <- function(name, value) {
  assign(name, value, envir = `_renv_state`, inherits = FALSE)
}

renv_active_project_get <- function() {
  renv_state_get("project", getwd())
}

renv_active_project_set <- function(project) {
  renv_state_set("project", normalizePath(project, winslash = "/"))
}

renv_active_environment_get <- function() {
  renv_state_get("environment", "")
}

renv_active_environment_set <- function(environment) {
  renv_state_set("environment", environment)
}

renv_active_local_get <- function() {
  renv_state_get("local", FALSE)
}

renv_active_local_set <- function(local) {
  renv_state_set("local", local)
}



renv_active_manifest <- function(project = NULL) {
  project <- project %||% renv_active_project_get()
  path <- file.path(project, "renv/manifest")
  manifests <- list.files(path, full.names = TRUE, recursive = TRUE)
  if (empty(manifests)) "" else tail(sort(manifests), n = 1L)
}
