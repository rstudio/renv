ensure_existing_renv <- function(name) {

  path <- renv_paths_config(name)
  if (!file.exists(path)) {
    fmt <- "Virtual environment '%s' does not exist."
    stopf(fmt, name)
  }

  invisible(path)
}

ensure_no_renv <- function(name, path) {

  path <- renv_paths_config(name)
  if (file.exists(path)) {
    fmt <- "Virtual environment '%s' already exists."
    stopf(fmt, name)
  }

  invisible(path)

}
