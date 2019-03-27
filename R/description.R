
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

  # if given a tarball, attempt to extract inner DESCRIPTION file
  ext <- tools::file_ext(path)
  if (ext %in% c("tar", "gz", "tgz")) {

    # list files within the archive
    files <- untar(path, list = TRUE)

    # find the DESCRIPTION file. note that for some source tarballs (e.g.
    # those from GitHub) the first entry may not be the package name, so
    # just consume everything up to the first slash
    file <- grep("^[^/]+/DESCRIPTION$", files, value = TRUE)
    if (length(file) != 1)
      stopf("failed to infer path to DESCRIPTION within file '%s'", path)

    # unpack into tempdir location
    exdir <- tempfile("renv-description-")
    on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
    untar(path, files = file, exdir = exdir)

    # update path to extracted DESCRIPTION
    path <- file.path(exdir, file)
  }

  # if given a zip, attempt to extract inner DESCRIPTION file
  if (ext %in% c("zip")) {

    meta <- unzip(path, list = TRUE)
    files <- meta$Name

    # find the DESCRIPTION file. note that for some source tarballs (e.g.
    # those from GitHub) the first entry may not be the package name, so
    # just consume everything up to the first slash
    file <- grep("^[^/]+/DESCRIPTION$", files, value = TRUE)
    if (length(file) != 1)
      stopf("failed to infer path to DESCRIPTION within file '%s'", path)

    # unpack into tempdir location
    exdir <- tempfile("renv-description-")
    on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
    unzip(path, files = file, exdir = exdir)

    # update path to extracted DESCRIPTION
    path <- file.path(exdir, file)

  }

  dcf <- renv_dcf_read(path)
  renv_filebacked_set(key, dcf)
  dcf

}

renv_description_augment <- function(path, record, type = NULL) {

  path <- renv_description_path(path)

  # read remote fields
  fields <- c(
    RemoteType = type,
    record[grep("^Remote", names(record))]
  )

  # read DESCRIPTION contents
  contents <- readLines(path, warn = FALSE)

  # remove empty lines
  contents <- contents[nzchar(contents)]

  # remove old remote fields
  contents <- grep("^Remote", contents, invert = TRUE, value = TRUE)

  # add in new remote fields
  text <- paste(names(fields), fields, sep = ": ")
  contents <- c(contents, text)

  # write it back out
  writeLines(contents, con = path, useBytes = TRUE)

}

renv_description_path <- function(path) {
  childpath <- file.path(path, "DESCRIPTION")
  indirect <- file.exists(childpath)
  path[indirect] <- childpath[indirect]
  path
}
