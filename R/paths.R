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



renv_platform_prefix <- function(...) {
  file.path(R.version$platform, getRversion()[1, 1:2])
}
