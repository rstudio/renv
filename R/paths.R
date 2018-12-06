renv_paths_root <- function(..., local) {
  root <- Sys.getenv("RENV_PATHS_ROOT", renv_paths_root_default(local = local))
  file.path(root, ...)
}

renv_paths_lib <- function(..., local) {
  root <- Sys.getenv("RENV_PATHS_LIBRARY", renv_paths_root("library", local = local))
  file.path(root, renv_platform_prefix(), ...)
}

renv_paths_conf <- function(..., local) {
  root <- Sys.getenv("RENV_PATHS_CONFIG", renv_paths_root("config", local = local))
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



renv_platform_prefix <- function(...) {
  file.path(R.version$platform, getRversion()[1, 1:2])
}
