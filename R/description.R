
description_read <- function(path) {

  ext <- tools::file_ext(path)
  if (ext %in% c("tar", "gz", "tgz")) {

    # figure out package name from file name
    filename <- basename(path)
    parts <- strsplit(filename, "_", fixed = TRUE)[[1]]
    name <- parts[[1]]

    # construct path to inner DESCRIPTION
    inner <- file.path(name, "DESCRIPTION")

    # unpack into tempdir location
    untar(path, files = inner, exdir = tempdir())

    # update path to extracted DESCRIPTION
    path <- file.path(tempdir(), inner)
  }

  read.dcf(path, all = TRUE)

}
