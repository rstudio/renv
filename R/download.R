
download <- function(url, destfile, quiet = FALSE) {

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

  if (!quiet) vwritef("Retrieving '%s' ...", url)

  # if the file already exists, compare its size with
  # the server's reported size for that file
  size <- renv_download_size(url)
  if (size != -1 && renv_file_exists(destfile)) {
    if (file.size(destfile) == size) {
      messagef("\tOK [file is up to date]")
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

  status <-
    catch(download.file(
      url,
      destfile = tempfile,
      quiet    = TRUE,
      mode     = "wb",
      method   = renv_download_file_method(),
      extra    = renv_download_file_extra()
    ))

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
  if (!quiet)
    renv_download_report(after - before, file.size(tempfile))

  # move the file to the requested location
  renv_file_move(tempfile, destfile)

  # and return path to successfully retrieved file
  destfile

}

renv_download_prepare <- function(url) {

  # TODO: do we want to support plain http?
  # don't see a good reason to in 2019
  github_hosts <- c(
    "https://api.github.com/",
    "https://raw.githubusercontent.com/"
  )

  for (host in github_hosts)
    if (startswith(url, host))
      return(renv_download_prepare_github())

  gitlab_hosts <- c(
    "https://gitlab.com/"
  )

  for (host in gitlab_hosts)
    if (startswith(url, host))
      return(renv_download_prepare_gitlab())

  function() {}

}

renv_download_prepare_github <- function() {

  pat <- Sys.getenv("GITHUB_PAT", unset = NA)
  if (is.na(pat))
    return(function() {})

  headers <- list("Authorization" = paste("token", pat))
  renv_download_prepare_headers(headers)

}

renv_download_prepare_gitlab <- function() {

  pat <- Sys.getenv("GITLAB_PAT", unset = NA)
  if (is.na(pat))
    return(function() {})

  headers <- list("Private-Token" = pat)
  renv_download_prepare_headers(headers)

}

renv_download_prepare_headers <- function(headers) {

  headertext <- paste(
    "--header",
    shQuote(paste(names(headers), headers, sep = ": "))
  )

  # infer an appropriate download method
  if (nzchar(Sys.which("curl"))) {
    method <- "curl"
    extra <- c("--location", "--fail", headertext)
  } else if (nzchar(Sys.which("wget"))) {
    method <- "wget"
    extra <- headertext
  } else {
    return(function() {})
  }

  saved <- options("download.file.method", "download.file.extra")
  options(download.file.method = method, download.file.extra = extra)
  function() { do.call(base::options, saved) }

}

renv_download_headers <- function(url) {

  file <- tempfile("renv-headers-")
  on.exit(unlink(file), add = TRUE)

  # don't know how to download headers without curl / wget
  method <- renv_download_file_method()
  if (!method %in% c("curl", "wget"))
    return(list())

  # add extra arguments to request headers
  extra <- c(
    renv_download_file_extra(),
    if (method == "curl") c("--head", "--include"),
    if (method == "wget") c("--server-response", "--spider")
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

  # read the downloaded headers
  contents <- read(file)

  # if redirects were required, each set of headers will
  # be reported separately, so just report the final set
  # of headers (ie: ignore redirects)
  splat <- strsplit(contents, "\n\n", fixed = TRUE)[[1]]
  text <- strsplit(splat[[length(splat)]], "\n", fixed = TRUE)[[1]]

  # keep only header lines
  lines <- grep(":", text, fixed = TRUE, value = TRUE)
  headers <- catch(renv_read_properties(text = lines))
  names(headers) <- tolower(names(headers))
  if (inherits(headers, "error"))
    return(list())

  headers

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

renv_download_file_method <- function() {

  # respect user preference for curl / wget as we support these
  method <- getOption("download.file.method", default = "auto")
  if (method %in% c("curl", "wget"))
    return(method)

  # otherwise, try and find a method that exists
  if (nzchar(Sys.which("curl")))
    return("curl")

  if (nzchar(Sys.which("wget")))
    return("wget")

  # fall back to default method
  method

}

renv_download_file_extra <- function() {

  method <- renv_download_file_method()
  extra <- getOption("download.file.extra", default = character())

  if (method == "curl")
    extra <- unique(c("--location", "--fail", extra))

  extra

}

renv_download_report <- function(elapsed, size) {

  if (!renv_verbose())
    return()

  time <- round(elapsed, 1)
  size <- structure(size, class = "object_size")

  fmt <- "\tOK [downloaded %s in %s]"
  vwritef(fmt, format(size, units = "auto"), format(time, units = "auto"))

}
