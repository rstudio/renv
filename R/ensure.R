
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

ensure_directory <- function(paths) {

  # handle zero-path case
  if (empty(paths))
    return(invisible(paths))

  # collect file info as list
  fileinfo <- file.info(paths, extra_cols = FALSE)
  infos <- lapply(1:nrow(fileinfo), function(i) fileinfo[i, ])

  # check for existing files that aren't directories
  for (info in infos)
    if (identical(info$isdir, FALSE))
      stopf("path '%s' exists but is not a directory", rownames(info))

  # create missing directories
  for (info in infos)
    if (is.na(info$isdir))
      if (!dir.create(rownames(info), recursive = TRUE))
        stopf("failed to create directory at path '%s'", rownames(info))

  # return the paths
  invisible(paths)

}

ensure_parent_directory <- function(path) {
  ensure_directory(unique(dirname(path)))
}

