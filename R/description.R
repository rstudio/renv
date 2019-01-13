
renv_description_read <- function(path, package = NULL) {

  # if given a package name, construct path to that package
  if (!is.null(package))
    path <- find.package(package)

  # accept package directories
  if (file.exists(file.path(path, "DESCRIPTION")))
    path <- file.path(path, "DESCRIPTION")

  # ensure that we have a real file
  info <- file.info(path, extra_cols = FALSE)
  if (is.na(info$isdir))
    stopf("file '%s' does not exist.", path)
  else if (info$isdir)
    stopf("file '%s' is a directory.", path)

  # check for a cache entry
  key <- path
  entry <- renv_filebacked_get(key)
  if (!is.null(entry))
    return(entry)

  # if given a tarball, attempt to extract inner DESCRIPTION file
  ext <- tools::file_ext(path)
  if (ext %in% c("tar", "gz", "tgz")) {

    # list files within the archive
    files <- untar(path, list = TRUE)

    # find the DESCRIPTION file
    file <- grep("^[a-zA-Z0-9._]+/DESCRIPTION$", files, value = TRUE)
    if (length(file) != 1)
      stopf("Failed to infer path to DESCRIPTION within file '%s'", path)

    # unpack into tempdir location
    exdir <- tempfile("description-")
    on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
    untar(path, files = file, exdir = exdir)

    # update path to extracted DESCRIPTION
    path <- file.path(exdir, file)
  }

  dcf <- renv_dcf_read(path)
  renv_filebacked_set(key, dcf)
  dcf

}
