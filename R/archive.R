
renv_archive_type <- function(path) {

  ext <- fileext(path)

  if (ext %in% c(".tgz", ".tar", ".tar.gz"))
    return("tar")
  else if (ext %in% c(".zip"))
    return("zip")
  else
    return("unknown")

}

renv_archive_list <- function(path) {
  suppressWarnings(renv_archive_list_impl(path))
}

renv_archive_list_impl <- function(path) {

  switch(
    renv_archive_type(path),
    tar = untar(path, list = TRUE),
    zip = unzip(path, list = TRUE)[["Name"]],
    stopf("don't know how to list files in archive '%s'", basename(path))
  )

}

renv_archive_decompress <- function(path, files = NULL, exdir = ".", ...) {

  switch(
    renv_archive_type(path),
    tar = renv_archive_decompress_tar(path, files = files, exdir = exdir, ...),
    zip = renv_archive_decompress_zip(path, files = files, exdir = exdir, ...),
    stopf("don't know how to decompress archive '%s'", basename(path))
  )

}

renv_archive_decompress_tar <- function(path, files = NULL, exdir = ".", ...) {

  # when using internal TAR, we want to suppress warnings
  # (otherwise we get noise about global PAX headers)
  suppressWarnings(untar(path, files = files, exdir = exdir, tar = "internal", ...))
  return(TRUE)

}

renv_archive_decompress_zip <- function(path, files = NULL, exdir = ".", ...) {

  # the default unzip tool will give warnings rather than
  # errors if R was unable to extract from a zip archive
  status <- tryCatch(
    unzip(path, files = files, exdir = exdir, ...),
    condition = identity
  )

  if (inherits(status, "condition")) {
    fmt <- "failed to decompress '%s' [%s]"
    message <- sprintf(fmt, basename(path), conditionMessage(status))
    stop(simpleError(message))
  }

  TRUE

}

renv_archive_find <- function(path, pattern) {
  files <- renv_archive_list(path)
  grep(pattern, files, value = TRUE)
}

renv_archive_read <- function(path, file) {

  type <- renv_archive_type(path)
  case(
    type == "tar" ~ renv_archive_read_tar(path, file),
    type == "zip" ~ renv_archive_read_zip(path, file),
    ~ stopf("don't know how to read file from archive %s", renv_path_pretty(path))
  )

}

renv_archive_read_tar <- function(path, file) {

  # if an appropriate tar is available, use it
  tar <- renv_tar_exe()
  if (nzchar(tar)) {
    args <- c("xf", shQuote(path), "-O", shQuote(file))
    return(renv_system_exec(tar, args, action = "reading file from archive"))
  }

  # create extraction directory
  exdir <- renv_scope_tempfile("renv-archive-")
  ensure_directory(exdir)

  # unpack the requested file
  suppressWarnings(untar(path, files = file, exdir = exdir, tar = "internal"))

  # and read it
  path <- file.path(exdir, file)
  readLines(path, warn = FALSE)

}

renv_archive_read_zip <- function(path, file) {
  renv_scope_tempdir()
  conn <- unz(path, file, encoding = "native.enc")
  on.exit(close(conn), add = TRUE)
  readLines(conn, warn = FALSE)
}
