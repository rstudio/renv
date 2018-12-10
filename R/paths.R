
renv_paths_root <- function(...) {
  root <- Sys.getenv("RENV_PATHS_ROOT", renv_paths_root_default())
  file.path(root, ...)
}

renv_paths_library <- function(...) {
  root <- Sys.getenv("RENV_PATHS_LIBRARY", renv_paths_root("library"))
  file.path(root, renv_platform_prefix(), ...)
}

renv_paths_config <- function(...) {
  root <- Sys.getenv("RENV_PATHS_CONFIG", renv_paths_root("config"))
  file.path(root, ...)
}

renv_paths_cache <- function(...) {
  root <- Sys.getenv("RENV_PATHS_CACHE", renv_paths_root("cache"))
  file.path(root, ...)
}

renv_paths_renv <- function(...) {
  root <- Sys.getenv("RENV_PATHS_RENV", renv_paths_root("renv"))
  file.path(root, renv_platform_prefix(), ...)
}

renv_paths_root_default <- function() {
  if (renv_local())
    file.path(renv_active_project(), ".renv")
  else
    path.expand("~/.renv")
}



renv_platform_prefix <- function(...) {
  file.path(R.version$platform, getRversion()[1, 1:2])
}
