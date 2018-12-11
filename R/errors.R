
ensure_existing_renv <- function(name) {

  path <- renv_paths_environments(name)
  if (!file.exists(path)) {
    fmt <- "%s environment '%s' does not exist."
    stopf(fmt, if (renv_local()) "Local virtual" else "Virtual", name)
  }

  invisible(path)
}

ensure_no_renv <- function(name) {

  path <- renv_paths_environments(name)
  if (file.exists(path)) {
    fmt <- "%s environment '%s' already exists."
    stopf(fmt, if (renv_local()) "Local virtual" else "Virtual", name)
  }

  invisible(path)

}
