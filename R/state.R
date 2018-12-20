
renv_active_project_get <- function() {
  getOption("renv.active.project", default = getwd())
}

renv_active_project_set <- function(project) {
  options("renv.active.project" = normalizePath(project, winslash = "/"))
}

renv_active_environment_get <- function() {
  getOption("renv.active.environment", default = "")
}

renv_active_environment_set <- function(environment) {
  options("renv.active.environment" = environment)
}

renv_active_local_get <- function() {
  getOption("renv.active.local", FALSE)
}

renv_active_local_set <- function(local) {
  options("renv.active.local" = local)
}



renv_active_manifest <- function(project = NULL) {
  project <- project %||% renv_active_project_get()
  path <- file.path(project, "renv/manifest")
  manifests <- list.files(path, full.names = TRUE, recursive = TRUE)
  if (empty(manifests)) "" else tail(sort(manifests), n = 1L)
}
