
renv_description_read <- function(path = NULL, package = NULL, subdir = NULL, ...) {

  # if given a package name, construct path to that package
  path <- path %||% find.package(package)
  stopifnot(renv_path_absolute(path))

  # accept package directories
  path <- renv_description_path(path)

  # read value with filebacked cache
  renv_filebacked(
    "DESCRIPTION", path,
    renv_description_read_impl,
    subdir = subdir, ...
  )

}

renv_description_read_impl <- function(path = NULL, subdir = NULL, ...) {

  # ensure that we have a real file
  info <- file.info(path, extra_cols = FALSE)
  if (is.na(info$isdir))
    stopf("file '%s' does not exist", path)
  else if (info$isdir)
    stopf("file '%s' exists but is a directory", path)

  # if we have an archive, attempt to unpack the DESCRIPTION
  type <- renv_archive_type(path)
  if (type != "unknown") {

    # list files within the archive
    files <- renv_archive_list(path)

    # find the DESCRIPTION file. note that for some source tarballs (e.g.
    # those from GitHub) the first entry may not be the package name, so
    # just consume everything up to the first slash
    subdir <- subdir %||% ""
    parts <- c("^[^/]+", if (nzchar(subdir)) subdir, "DESCRIPTION$")

    pattern <- paste(parts, collapse = "/")

    descs <- grep(pattern, files, value = TRUE)
    if (empty(descs)) {
      fmt <- "archive '%s' does not appear to contain a DESCRIPTION file"
      stopf(fmt, aliased_path(path))
    }

    # choose the shortest DESCRPITION file matching
    # unpack into tempdir location
    file <- descs[[1]]
    exdir <- renv_tempfile("renv-description-")
    renv_archive_decompress(path, files = file, exdir = exdir)

    # update path to extracted DESCRIPTION
    path <- file.path(exdir, file)

  }

  dcf <- renv_dcf_read(path, ...)
  if (empty(dcf))
    stopf("DESCRIPTION file at '%s' is empty", path)

  if (identical(dcf$Encoding, "UTF-8"))
    dcf[] <- lapply(dcf, renv_encoding_mark, encoding = "UTF-8")

  dcf

}

renv_description_path <- function(path) {
  childpath <- file.path(path, "DESCRIPTION")
  indirect <- file.exists(childpath)
  path[indirect] <- childpath[indirect]
  path
}
