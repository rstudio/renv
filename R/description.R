
renv_description_read <- function(path = NULL, package = NULL) {

  # if given a package name, construct path to that package
  if (!is.null(package))
    path <- find.package(package)

  # accept package directories
  path <- renv_description_path(path)

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

  # if we have an archive, attempt to unpack the DESCRIPTION
  type <- renv_archive_type(path)
  if (type != "unknown") {

    # list files within the archive
    files <- renv_archive_list(path)

    # find the DESCRIPTION file. note that for some source tarballs (e.g.
    # those from GitHub) the first entry may not be the package name, so
    # just consume everything up to the first slash
    file <- grep("^[^/]+/DESCRIPTION$", files, value = TRUE)
    if (length(file) != 1)
      stopf("failed to infer path to DESCRIPTION within file '%s'", path)

    # unpack into tempdir location
    exdir <- renv_file_temp("renv-description-")

    decompress <- renv_archive_decompressor(path)
    decompress(path, files = file, exdir = exdir)

    # update path to extracted DESCRIPTION
    path <- file.path(exdir, file)

  }

  dcf <- renv_dcf_read(path)
  renv_filebacked_set(key, dcf)
  dcf

}

renv_description_path <- function(path) {
  childpath <- file.path(path, "DESCRIPTION")
  indirect <- file.exists(childpath)
  path[indirect] <- childpath[indirect]
  path
}
