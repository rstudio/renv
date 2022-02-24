
renv_archive_type <- function(archive) {

  ext <- fileext(archive)

  if (ext %in% c(".tgz", ".tar", ".tar.gz"))
    return("tar")
  else if (ext %in% c(".zip"))
    return("zip")
  else
    return("unknown")

}

renv_archive_list <- function(archive) {
  suppressWarnings(renv_archive_list_impl(archive))
}

renv_archive_list_impl <- function(archive) {

  switch(
    renv_archive_type(archive),
    tar = untar(archive, list = TRUE),
    zip = unzip(archive, list = TRUE)[["Name"]],
    stopf("don't know how to list files in archive '%s'", basename(archive))
  )

}

renv_archive_decompress <- function(archive, files = NULL, exdir = ".", ...) {

  switch(
    renv_archive_type(archive),
    tar = renv_archive_decompress_tar(archive, files = files, exdir = exdir, ...),
    zip = renv_archive_decompress_zip(archive, files = files, exdir = exdir, ...),
    stopf("don't know how to decompress archive '%s'", basename(archive))
  )

}

renv_archive_decompress_tar <- function(archive, files = NULL, exdir = ".", ...) {

  # if an appropriate system tar is available, use it
  tar <- renv_tar_exe()
  if (nzchar(tar))
    return(renv_tar_decompress(tar, archive = archive, files = files, exdir = exdir, ...))

  # when using internal TAR, we want to suppress warnings
  # (otherwise we get noise about global PAX headers)
  suppressWarnings(untar(archive, files = files, exdir = exdir, tar = "internal", ...))
  return(TRUE)

}

renv_archive_decompress_zip <- function(archive, files = NULL, exdir = ".", ...) {

  # the default unzip tool will give warnings rather than
  # errors if R was unable to extract from a zip archive
  status <- tryCatch(
    unzip(archive, files = files, exdir = exdir, ...),
    condition = identity
  )

  if (inherits(status, "condition")) {
    fmt <- "failed to decompress '%s' [%s]"
    message <- sprintf(fmt, basename(archive), conditionMessage(status))
    stop(simpleError(message))
  }

  TRUE

}

renv_archive_find <- function(archive, pattern) {
  files <- renv_archive_list(archive)
  grep(pattern, files, value = TRUE)
}

renv_archive_read <- function(archive, file) {

  type <- renv_archive_type(archive)
  case(
    type == "tar" ~ renv_archive_read_tar(archive, file),
    type == "zip" ~ renv_archive_read_zip(archive, file),
    ~ stopf("don't know how to read file from archive %s", renv_path_pretty(archive))
  )

}

renv_archive_read_tar <- function(archive, file) {

  # if an appropriate tar is available, use it
  tar <- renv_tar_exe()
  if (nzchar(tar)) {
    args <- c("xf", shQuote(archive), "-O", shQuote(file))
    return(renv_system_exec(tar, args, action = "reading file from archive"))
  }

  # create extraction directory
  exdir <- renv_scope_tempfile("renv-archive-")
  ensure_directory(exdir)

  # unpack the requested file
  suppressWarnings(untar(archive, files = file, exdir = exdir, tar = "internal"))

  # and read it
  archive <- file.path(exdir, file)
  readLines(archive, warn = FALSE)

}

renv_archive_read_zip <- function(archive, file) {
  renv_scope_tempdir()
  conn <- unz(archive, file, encoding = "native.enc")
  on.exit(close(conn), add = TRUE)
  readLines(conn, warn = FALSE)
}
