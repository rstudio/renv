
renv_description_read <- function(path) {

  # accept package directories
  if (file.exists(file.path(path, "DESCRIPTION")))
    path <- file.path(path, "DESCRIPTION")

  # ensure that we have a real file
  info <- file.info(path, extra_cols = FALSE)
  case(
    is.na(info$isdir)  ~ stopf("file '%s' does not exist.", path),
    isTRUE(info$isdir) ~ stopf("file '%s' is a directory.", path)
  )

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

  # read it
  renv_dcf_read(path)

}
