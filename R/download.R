
download <- function(url, destfile = tempfile()) {

  # handle local files by just copying the file
  if (grepl("file:", url)) {
    source <- sub("^file:(?://)?", "", url)
    renv_file_copy(source, destfile, overwrite = TRUE)
    return(destfile)
  }

  vmessagef("Retrieving '%s' ...", url)

  # if the file already exists, compare its size with
  # the server's reported size for that file
  headers <- renv_download_headers(url)
  if (renv_file_exists(destfile)) {
    size <- file.size(destfile)
    reported <- as.numeric(headers$`Content-Length`)
    if (size == reported) {
      messagef("\tOK [file is up-to-date]")
      return(destfile)
    }
  }

  # back up a pre-existing file if necessary
  callback <- renv_file_scoped_backup(destfile)
  on.exit(callback(), add = TRUE)

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

  # double-check that the reported size is correct
  size <- file.size(destfile)
  reported <- as.numeric(headers$`Content-Length`)
  if (size != reported)
    stopf("download failed [file was truncated]")

  # everything looks ok: report success
  if (renv_verbose()) {
    size <- structure(file.info(destfile)$size, class = "object_size")
    time <- round(after - before, 1)
    fmt <- "\tOK [downloaded %s in %s]"
    messagef(fmt, format(size, units = "auto"), format(time, units = "auto"))
  }

  destfile

}

renv_download_headers <- function(url) {
  case(
    nzchar(Sys.which("curl")) ~ renv_download_headers_curl(url),
    nzchar(Sys.which("wget")) ~ renv_download_headers_wget(url)
  )
}

renv_download_headers_curl <- function(url) {
  fmt <- "%s --silent --location --head %s 2>&1"
  cmd <- sprintf(fmt, shQuote(Sys.which("curl")), shQuote(url))
  output <- trimws(system(cmd, intern = TRUE))
  renv_dcf_read(textConnection(output[-1L]))
}

renv_download_headers_wget <- function(url) {
  fmt <- "%s --quiet --server-response --spider %s 2>&1"
  cmd <- sprintf(fmt, shQuote(Sys.which("wget")), shQuote(url))
  output <- trimws(system(cmd, intern = TRUE))
  renv_dcf_read(textConnection(output[-1L]))
}
