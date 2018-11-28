renv_paths_root <- function(...) {
  root <- Sys.getenv("RENV_PATHS_ROOT", path.expand("~/.renv"))
  file.path(root, ...)
}

renv_paths_lib <- function(...) {
  root <- Sys.getenv("RENV_PATHS_LIB", renv_paths_root("lib", renv_platform_prefix()))
  file.path(root, ...)
}

renv_paths_conf <- function(...) {
  root <- Sys.getenv("RENV_PATHS_CONF", renv_paths_root("conf"))
  file.path(root, ...)
}



renv_platform_prefix <- function(...) {
  file.path(R.version$platform, getRversion()[1, 1:2])
}
