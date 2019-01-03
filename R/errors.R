
ensure_existing_path <- function(path) {
  if (!file.exists(path))
    stopf("no file at path '%s'", path)
  invisible(path)
}

ensure_existing_file <- function(path) {
  info <- file.info(path, extra_cols = FALSE)
  if (is.na(info$isdir))
    stopf("no file at path '%s'", path)
  else if (identical(info$isdir, TRUE))
    stopf("file '%s' exists but is a directory")
  invisible(path)
}

ensure_existing_renv <- function(name) {

  path <- renv_paths_environment(name)
  if (!file.exists(path)) {
    fmt <- "%s environment '%s' does not exist."
    stopf(fmt, if (renv_state$local()) "Local virtual" else "Virtual", name)
  }

  invisible(path)
}

ensure_no_renv <- function(name) {

  path <- renv_paths_environment(name)
  if (file.exists(path)) {
    fmt <- "%s environment '%s' already exists."
    stopf(fmt, if (renv_state$local()) "Local virtual" else "Virtual", name)
  }

  invisible(path)

}

ensure_directory <- function(path) {

  info <- file.info(path)
  if (identical(info$isdir, FALSE))
    stopf("path '%s' exists but is not a directory", path)

  if (is.na(info$isdir)) {
    if (!dir.create(path, recursive = TRUE))
      stopf("failed to create directory at path '%s'", path)
  }

  invisible(path)

}

ensure_parent_directory <- function(path) {
  ensure_directory(dirname(path))
}

