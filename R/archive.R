
renv_archive_type <- function(path) {

  ext <- fileext(path)
  if (ext %in% c(".tgz", ".tar", ".tar.gz"))
    return("tar")
  if (ext %in% c(".zip"))
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

renv_archive_decompress <- function(path, exdir = ".", ...) {

  switch(
    renv_archive_type(path),
    tar = renv_archive_decompress_tar(path, exdir = exdir, ...),
    zip = renv_archive_decompress_zip(path, exdir = exdir, ...),
    stopf("don't know how to decompress archive '%s'", basename(path))
  )

}

renv_archive_decompress_tar_find <- function() {

  # check for tar in envvar -- allow for explicitly-requested
  # internal tar as well
  tar <- Sys.getenv("TAR", unset = NA)
  if (identical(tar, "internal"))
    return(NULL)

  # if the requested tar exists, use it
  if (!is.na(tar) && nzchar(Sys.which(tar)))
    return(tar)

  # no TAR envvar set; try looking for tar on the PATH
  # TODO: is this safe on Windows? what if a bad tar is on the PATH?
  tar <- Sys.which("tar")
  if (nzchar(tar))
    return(tar)

  # no tar found
  NULL

}

renv_archive_decompress_tar <- function(path, exdir = ".", ...) {


  # when using internal TAR, we want to suppress warnings
  # (otherwise we get noise about global PAX headers)
  tar <- renv_archive_decompress_tar_find()
  if (is.null(tar)) {
    suppressWarnings(untar(path, exdir = exdir, tar = "internal", ...))
    return(TRUE)
  }

  # TODO: is it safe to use an external tar on Windows?
  # should we validate that the version of tar.exe found
  # on the PATH is okay?

  # construct arguments for archive extraction
  path <- renv_path_normalize(path, winslash = "/", mustWork = TRUE)
  args <- c("xf", shQuote(path))

  # add in extraction dir
  if (exdir != ".") {
    ensure_directory(exdir)
    exargs <- c("-C", shQuote(renv_path_normalize(exdir, winslash = "/", mustWork = TRUE)))
    args <- c(args, exargs)
  }

  # execute the command
  status <- system2(tar, args, stdout = FALSE, stderr = FALSE)
  if (status != 0L)
    stopf("error decompressing '%s' [error code %i]", basename(path), status)

  TRUE

}

renv_archive_decompress_zip <- function(path, ...) {

  # the default unzip tool will give warnings rather than
  # errors if R was unable to extract from a zip archive
  status <- tryCatch(unzip(path, ...), condition = identity)
  if (inherits(status, "condition")) {
    fmt <- "failed to decompress '%s' [%s]"
    message <- sprintf(fmt, basename(path), conditionMessage(status))
    stop(simpleError(message))
  }

  TRUE

}

