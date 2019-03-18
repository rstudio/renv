
download <- function(url,
                     destfile,
                     quiet    = FALSE,
                     method   = getOption("download.file.method"),
                     extra    = getOption("download.file.extra"))
{
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
  size <- renv_download_size(url, method, extra)
  if (size != -1 && renv_file_exists(destfile)) {
    if (file.size(destfile) == size) {
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
  if (size != -1 && file.size(tempfile) != size)
    stopf("download failed [file was truncated]")

  # everything looks ok: report success
  if (renv_verbose()) {
    size <- structure(file.size(tempfile), class = "object_size")
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

  auth <- paste("Authorization: token", pat)
  extra <- c("--location", "--fail", "--header", shQuote(auth))
  saved <- options("download.file.method", "download.file.extra")
  options(download.file.method = "curl", download.file.extra = extra)
  function() { do.call(base::options, saved) }

}

renv_download_headers <- function(url, method, extra) {

  file <- tempfile("renv-headers-")
  on.exit(unlink(file), add = TRUE)

  # don't know how to download headers without curl / wget
  if (!method %in% c("curl", "wget"))
    return(list())

  # add extra arguments to request headers
  extra <- c(
    extra,
    if (method == "curl") c("--silent", "--head"),
    if (method == "wget") c("--quiet", "--server-response", "--spider")
  )

  # perform the download
  status <- download.file(
    url = url,
    destfile = file,
    quiet = TRUE,
    mode = "wb",
    method = method,
    extra = extra
  )

  dcf <- catch(renv_dcf_read(file))
  if (inherits(dcf, "error"))
    return(list())

  dcf

}

renv_download_size <- function(url, method, extra) {

  headers <- catch(renv_download_headers(url, method, extra))
  if (inherits(headers, "error"))
    return(-1)

  size <- headers[["content-length"]]
  if (is.null(size))
    return(-1)

  as.numeric(size)

}
