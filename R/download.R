
# download a file from 'url' to file 'destfile'. the 'type'
# argument tells us the remote type, which is used to motivate
# what form of authentication is appropriate; the 'quiet'
# argument is used to display / suppress output
download <- function(url, destfile, type = NULL, quiet = FALSE) {

  # handle local files by just copying the file
  if (grepl("^file:", url)) {
    source <- sub("^file:(?://)?", "", url)
    renv_file_copy(source, destfile, overwrite = TRUE)
    return(destfile)
  }

  if (!quiet) vwritef("Retrieving '%s' ...", url)

  # if the file already exists, compare its size with
  # the server's reported size for that file
  size <- renv_download_size(url, type)
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
  tempfile <- renv_file_temp(tmpdir = dirname(destfile))

  # request the download
  before <- Sys.time()
  status <- renv_download_impl(url, destfile = tempfile, type = type, headers = FALSE)
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

renv_download_impl <- function(url, destfile, type = NULL, headers = FALSE) {

  downloader <- switch(
    renv_download_file_method(),
    curl = renv_download_curl,
    wget = renv_download_wget
  )

  catch(downloader(url, destfile, type, headers))

}

renv_download_curl <- function(url, destfile, type, headers) {

  config <- renv_file_temp("renv-download-config-")

  fields <- c(url = url, output = destfile)
  fields <- c(fields, renv_download_auth(url, type))

  keys <- names(fields)
  vals <- shQuote(fields, type = "cmd")
  text <- paste(keys, vals, sep = " = ")

  flags <- c("location", "fail", "silent", "show-error")
  if (headers)
    flags <- c(flags, "head", "include")

  text <- c(text, flags)

  umask <- Sys.umask("077")
  on.exit(Sys.umask(umask), add = TRUE)

  writeLines(text, con = config)
  system2("curl", c("--config", shQuote(config)))

}

renv_download_wget <- function(url, destfile, type, headers) {

  config <- renv_file_temp("renv-download-config-")

  fields <- c(quiet = "on")
  fields <- c(fields, renv_download_auth(url, type))

  keys <- names(fields)
  vals <- unlist(fields)
  text <- paste(keys, vals, sep = " = ")

  umask <- Sys.umask("077")
  on.exit(Sys.umask(umask), add = TRUE)

  writeLines(text, con = config)

  args <- c("--config", shQuote(config))
  args <- c(args, shQuote(url), "-O", shQuote(destfile))
  system2("wget", args)

}

renv_download_auth_type <- function(url) {

  github_hosts <- c(
    "https://api.github.com/",
    "https://raw.githubusercontent.com/"
  )

  for (host in github_hosts)
    if (startswith(url, host))
      return("github")

  gitlab_hosts <- c(
    "https://gitlab.com/"
  )

  for (host in gitlab_hosts)
    if (startswith(url, host))
      return("gitlab")

  bitbucket_hosts <- c(
    "https://api.bitbucket.org/",
    "https://bitbucket.org/"
  )

  for (host in bitbucket_hosts)
    if (startswith(url, host))
      return("bitbucket")

  "unknown"

}

renv_download_auth <- function(url, type) {

  type <- type %||% renv_download_auth_type(url)
  switch(
    type,
    bitbucket = renv_download_auth_bitbucket(),
    github = renv_download_auth_github(),
    gitlab = renv_download_auth_gitlab(),
    character()
  )

}

renv_download_auth_bitbucket <- function() {

  user <- Sys.getenv("BITBUCKET_USER", unset = NA)
  pass <- Sys.getenv("BITBUCKET_PASSWORD", unset = NA)
  if (is.na(user) || is.na(pass))
    return(character())

  userpass <- paste(user, pass, sep = ":")
  auth <- paste("Basic", renv_base64_encode(userpass))
  c(header = paste("Authorization", auth, sep = ": "))

}

renv_download_auth_github <- function() {

  pat <- Sys.getenv("GITHUB_PAT", unset = NA)
  if (is.na(pat))
    return(character())

  token <- paste("token", pat)
  c(header = paste("Authorization", token, sep = ": "))

}

renv_download_auth_gitlab <- function() {

  pat <- Sys.getenv("GITLAB_PAT", unset = NA)
  if (is.na(pat))
    return(character())

  c(header = paste("Private-Token", pat, sep = ": "))

}

renv_download_headers <- function(url, type) {

  # can't download without curl or wget
  method <- renv_download_file_method()
  if (!method %in% c("curl", "wget"))
    return(list())

  file <- renv_file_temp("renv-headers-")

  # perform the download
  renv_download_impl(url, file, type, headers = TRUE)

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

renv_download_size <- function(url, type) {

  headers <- catch(renv_download_headers(url, type))
  if (inherits(headers, "error"))
    return(-1)

  size <- headers[["content-length"]]
  if (is.null(size))
    return(-1)

  as.numeric(size)

}

renv_download_file_method <- function() {

  method <- getOption("download.file.method", default = "auto")
  if (method %in% c("curl", "wget"))
    return(method)

  if (nzchar(Sys.which("curl")))
    return("curl")

  if (nzchar(Sys.which("wget")))
    return("wget")

  # fall back to default method
  method

}

renv_download_report <- function(elapsed, size) {

  if (!renv_verbose())
    return()

  time <- round(elapsed, 1)
  size <- structure(size, class = "object_size")

  fmt <- "\tOK [downloaded %s in %s]"
  vwritef(fmt, format(size, units = "auto"), format(time, units = "auto"))

}
