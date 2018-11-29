renv_paths_root <- function(...) {
  root <- Sys.getenv("RENV_PATHS_ROOT", path.expand("~/.renv"))
  file.path(root, ...)
}

renv_paths_lib <- function(...) {
  root <- Sys.getenv("RENV_PATHS_LIBRARY", renv_paths_root("lib"))
  file.path(root, renv_platform_prefix(), ...)
}

renv_paths_conf <- function(...) {
  root <- Sys.getenv("RENV_PATHS_CONFIG", renv_paths_root("conf"))
  file.path(root, ...)
}

renv_paths_renv <- function(...) {
  root <- Sys.getenv("RENV_PATHS_RENV", renv_paths_root("renv"))
  file.path(root, renv_platform_prefix(), ...)
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
