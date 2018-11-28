ensure_existing_renv <- function(name) {

  path <- renv_paths_conf(name)
  if (!file.exists(path)) {
    fmt <- "no renv definition found at path '%s'"
    stopf(fmt, aliased_path(path))
  }

  invisible(path)
}

ensure_no_renv <- function(name, path) {

  path <- renv_paths_conf(name)
  if (file.exists(path)) {
    fmt <- "renv already exists at path '%s'"
    stopf(fmt, name, aliased_path(path))
  }

  invisible(path)

}
