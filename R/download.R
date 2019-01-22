
download <- function(url, destfile = tempfile()) {

  # handle local files by just copying the file
  if (grepl("file:", url)) {
    source <- sub("^file:[/\\]*", "", url)
    renv_file_copy(source, destfile, overwrite = TRUE)
    return(destfile)
  }

  vmessagef("Retrieving '%s' ...", url)

  before <- Sys.time()
  status <- download.file(url, destfile, quiet = TRUE, mode = "wb")
  after <- Sys.time()
  if (status != 0)
    stopf("Download failed [error code %i]", status)

  if (!renv_file_exists(destfile))
    stopf("Download failed [unknown reason]")

  if (renv_verbose()) {
    size <- structure(file.info(destfile)$size, class = "object_size")
    time <- round(after - before, 1)
    fmt <- "\tOK [downloaded %s in %s]"
    messagef(fmt, format(size, units = "auto"), format(time, units = "auto"))
  }

  destfile

}
