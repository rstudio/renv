
download <- function(url, destfile = tempfile()) {

  # handle local files by just copying the file
  if (grepl("file:", url)) {
    source <- sub("^file:(?://)?", "", url)
    renv_file_copy(source, destfile, overwrite = TRUE)
    return(destfile)
  }

  vmessagef("Retrieving '%s' ...", url)

  # back up a pre-existing file if necessary
  callback <- renv_file_scoped_backup(destfile)
  on.exit(callback(), add = TRUE)

  # TODO: if we already have a file on-disk, perhaps we can
  # check if the file size matches the reported file size
  # from the server and skip the download in that case?

  # request the download
  before <- Sys.time()
  status <- catch(download.file(url, destfile, quiet = TRUE, mode = "wb"))
  after <- Sys.time()

  # check for failure
  if (inherits(status, "error")) {
    unlink(destfile, recursive = TRUE)
    stopf("download failed [%s]", conditionMessage(status))
  }

  if (status != 0L) {
    unlink(destfile, recursive = TRUE)
    stopf("download failed [error code %i]", status)
  }

  if (!renv_file_exists(destfile))
    stopf("download failed [unknown reason]")

  # everything looks ok: report success
  if (renv_verbose()) {
    size <- structure(file.info(destfile)$size, class = "object_size")
    time <- round(after - before, 1)
    fmt <- "\tOK [downloaded %s in %s]"
    messagef(fmt, format(size, units = "auto"), format(time, units = "auto"))
  }

  destfile

}
