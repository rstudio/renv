
ensure_existing_renv <- function(name) {

  path <- renv_paths_environment(name)
  if (!file.exists(path)) {
    fmt <- "%s environment '%s' does not exist."
    stopf(fmt, if (renv_active_local_get()) "Local virtual" else "Virtual", name)
  }

  invisible(path)
}

ensure_no_renv <- function(name) {

  path <- renv_paths_environment(name)
  if (file.exists(path)) {
    fmt <- "%s environment '%s' already exists."
    stopf(fmt, if (renv_active_local_get()) "Local virtual" else "Virtual", name)
  }

  invisible(path)

}
