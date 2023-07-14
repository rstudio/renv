
ensure_existing_path <- function(path) {
  if (!file.exists(path))
    stopf("no file at path '%s'", path)
  invisible(path)
}

ensure_existing_file <- function(path) {
  info <- renv_file_info(path)
  if (is.na(info$isdir))
    stopf("no file at path '%s'", path)
  else if (identical(info$isdir, TRUE))
    stopf("file '%s' exists but is a directory")
  invisible(path)
}

ensure_directory <- function(paths, umask = NULL) {

  # handle zero-path case
  if (empty(paths))
    return(invisible(paths))

  # set umask if necessary
  if (!is.null(umask))
    renv_scope_umask("0")

  # for each path, try to either create the directory, or assert that
  # the directory already exists. this should also help handle cases
  # where 'dir.create()' fails because another process created the
  # directory at the same time we attempted to do so
  for (path in paths) {

    ok <-
      dir.create(path, recursive = TRUE, showWarnings = FALSE) ||
      dir.exists(path)

    if (!ok)
      stopf("failed to create directory at path '%s'", path)

  }

  # return the paths
  invisible(paths)

}

ensure_parent_directory <- function(path) {
  ensure_directory(unique(dirname(path)))
}

