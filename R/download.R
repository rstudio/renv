
download <- function(url, destfile = tempfile(), quiet = FALSE) {

  # handle local files by just copying the file
  if (grepl("^file:", url)) {
    source <- sub("^file:(?://)?", "", url)
    renv_file_copy(source, destfile, overwrite = TRUE)
    return(destfile)
  }

  # prepare for the download (doing things like setting download.file.method,
  # download.file.extra as needed for the download source)
  prepare_callback <- renv_download_prepare(url)
  on.exit(prepare_callback(), add = TRUE)

  if (!quiet) vmessagef("Retrieving '%s' ...", url)

  # if the file already exists, compare its size with
  # the server's reported size for that file
  reported_size <- renv_download_size(url)
  if (reported_size != -1 && renv_file_exists(destfile)) {
    if (file.size(destfile) == reported_size) {
      messagef("\tOK [file is up-to-date]")
      return(destfile)
    }
  }

  # back up a pre-existing file if necessary
  backup_callback <- renv_file_scoped_backup(destfile)
  on.exit(backup_callback(), add = TRUE)

  # form path to temporary file
  tempfile <- tempfile("renv-download-", tmpdir = dirname(destfile))
  on.exit(unlink(tempfile), add = TRUE)

  # request the download
  before <- Sys.time()
  status <- catch(download.file(url, tempfile, quiet = TRUE, mode = "wb"))
  after <- Sys.time()

  # check for failure
  if (inherits(status, "error"))
    stopf("download failed [%s]", conditionMessage(status))

  if (status != 0L)
    stopf("download failed [error code %i]", status)

  if (!renv_file_exists(tempfile))
    stopf("download failed [unknown reason]")

  # double-check that the reported size is correct
  if (reported_size != -1 && file.size(tempfile) != reported_size)
    stopf("download failed [file was truncated]")

  # everything looks ok: report success
  if (renv_verbose()) {
    size <- structure(file.info(tempfile)$size, class = "object_size")
    time <- round(after - before, 1)
    fmt <- "\tOK [downloaded %s in %s]"
    if (!quiet) vmessagef(fmt, format(size, units = "auto"), format(time, units = "auto"))
  }

  # move the file to the requested location
  renv_file_move(tempfile, destfile)

  # and return path to successfully retrieved file
  destfile

}

renv_download_prepare <- function(url) {

  github_patterns <- c("api.github.com", "raw.githubusercontent.com")
  for (pattern in github_patterns)
    if (grepl(pattern, url, fixed = TRUE))
      return(renv_download_prepare_github(url))

  function() {}

}

renv_download_prepare_github <- function(url) {

  # do we have curl? if not, bail
  curl <- Sys.which("curl")
  if (!nzchar(curl))
    return(NULL)

  # do we have a GITHUB_PAT? if not, bail
  pat <- Sys.getenv("GITHUB_PAT", unset = NA)
  if (is.na(pat))
    return(NULL)

  extra <- sprintf("-L -f -H \"Authorization: token %s\"", pat)
  saved <- options("download.file.method", "download.file.extra")
  options(download.file.method = "curl", download.file.extra = extra)
  function() { do.call(base::options, saved) }

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
  dcf <- renv_dcf_read(textConnection(output[-1L]))
  names(dcf) <- tolower(names(dcf))
  dcf
}

renv_download_headers_wget <- function(url) {
  fmt <- "%s --quiet --server-response --spider %s 2>&1"
  cmd <- sprintf(fmt, shQuote(Sys.which("wget")), shQuote(url))
  output <- trimws(system(cmd, intern = TRUE))
  dcf <- renv_dcf_read(textConnection(output[-1L]))
  names(dcf) <- tolower(names(dcf))
  dcf
}

renv_download_size <- function(url) {

  headers <- catch(renv_download_headers(url))
  if (inherits(headers, "error"))
    return(-1)

  size <- headers[["content-length"]]
  if (is.null(size))
    return(-1)

  as.numeric(size)

}
