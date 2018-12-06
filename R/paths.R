renv_paths_root <- function(..., local) {
  root <- Sys.getenv("RENV_PATHS_ROOT", renv_paths_root_default(local = local))
  file.path(root, ...)
}

renv_paths_lib <- function(..., local) {
  root <- Sys.getenv("RENV_PATHS_LIBRARY", renv_paths_root("lib", local = local))
  file.path(root, renv_platform_prefix(), ...)
}

renv_paths_conf <- function(..., local) {
  root <- Sys.getenv("RENV_PATHS_CONFIG", renv_paths_root("conf", local = local))
  file.path(root, ...)
}

renv_paths_renv <- function(..., local) {
  root <- Sys.getenv("RENV_PATHS_RENV", renv_paths_root("renv", local = local))
  file.path(root, renv_platform_prefix(), ...)
}

renv_paths_root_default <- function(local) {
  if (local)
    file.path(renv_active_project(), ".renv")
  else
    path.expand("~/.renv")
}

renv_paths_subdir <- function() {
  "renv"
}

renv_paths_local <- function(project = NULL, ...) {
  root <- renv_active_project(project)
  file.path(root, renv_paths_subdir(), ...)
}

renv_paths_local_activate <- function() {
  file.path(renv_paths_subdir(), "activate.R")
}

renv_paths_local_active <- function() {
  file.path(renv_paths_subdir(), "active")
}

renv_paths_local_rbuildignore <- function() {
  sprintf("^%s/", renv_paths_subdir())
}




renv_platform_prefix <- function(...) {
  file.path(R.version$platform, getRversion()[1, 1:2])
}
