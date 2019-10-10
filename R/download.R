
# download a file from 'url' to file 'destfile'. the 'type'
# argument tells us the remote type, which is used to motivate
# what form of authentication is appropriate; the 'quiet'
# argument is used to display / suppress output. use 'headers'
# (as a named character vector) to supply additional headers
download <- function(url, destfile, type = NULL, quiet = FALSE, headers = NULL) {

  if (quiet)
    renv_scope_options(renv.verbose = FALSE)

  # handle local files by just copying the file
  if (grepl("^file:", url)) {
    source <- sub("^file:(?://)?", "", url)
    renv_file_copy(source, destfile, overwrite = TRUE)
    return(destfile)
  }

  # on Windows, try using our local curl binary if available
  renv_scope_downloader()

  vwritef("Retrieving '%s' ...", url)

  # if this file is a zipfile or tarball, rather than attempting
  # to download headers etc. to validate the file is okay, just
  # check that the archive appears not to be damaged
  status <- renv_download_check_archive(destfile)
  if (identical(status, TRUE)) {
    vwritef("\tOK [file is up to date]")
    return(destfile)
  }

  # if the file already exists, compare its size with
  # the server's reported size for that file
  size <- renv_download_size(url, type, headers)
  if (size != -1 && file.exists(destfile)) {
    if (file.size(destfile) == size) {
      vwritef("\tOK [file is up to date]")
      return(destfile)
    }
  }

  # back up a pre-existing file if necessary
  backup_callback <- renv_file_backup(destfile)
  on.exit(backup_callback(), add = TRUE)

  # form path to temporary file
  tempfile <- renv_tempfile(tmpdir = dirname(destfile))

  # request the download
  before <- Sys.time()

  status <- renv_download_impl(
    url = url,
    destfile = tempfile,
    type = type,
    request = "GET",
    headers = headers
  )

  after <- Sys.time()

  # check for failure
  if (inherits(status, "error"))
    stopf("download failed [%s]", conditionMessage(status))

  if (status != 0L)
    stopf("download failed [error code %i]", status)

  if (!file.exists(tempfile))
    stopf("download failed [unknown reason]")

  # double-check that the reported size is correct
  if (size != -1 && file.size(tempfile) != size)
    stopf("download failed [file was truncated]")

  # double-check archives are readable
  status <- renv_download_check_archive(tempfile)
  if (inherits(status, "error"))
    stopf("download failed [archive cannot be read]")

  # everything looks ok: report success
  renv_download_report(after - before, file.size(tempfile))

  # move the file to the requested location
  renv_file_move(tempfile, destfile)

  # and return path to successfully retrieved file
  destfile

}

# NOTE: only 'GET' and 'HEAD' are supported
renv_download_impl <- function(url, destfile, type = NULL, request = "GET", headers = NULL) {

  downloader <- switch(
    renv_download_file_method(),
    curl = renv_download_curl,
    wget = renv_download_wget,
    renv_download_default
  )

  catch(downloader(url, destfile, type, request, headers))

}

renv_download_default <- function(url, destfile, type, request, headers) {

  # custom request types are not supported with the default downloader
  if (request != "GET")
    stopf("the default downloader does not support %s requests", request)

  # try and ensure headers are set for older versions of R
  headers <- c(headers, renv_download_auth(url, type))
  renv_download_default_agent_scope(headers)

  # prefer the 'libcurl' method if available, but fall back
  # to 'auto' if not available
  method <- "auto"
  libcurl <- capabilities("libcurl")
  if (length(libcurl) && libcurl)
    method <- "libcurl"

  # handle absence of 'headers' argument in older versions of R
  args <- list(url      = url,
               destfile = destfile,
               method   = method,
               headers  = headers,
               mode     = "wb",
               quiet    = TRUE)

  fmls <- formals(download.file)
  args <- keep(args, names(fmls))

  do.call(download.file, args)

}

renv_download_default_agent_scope <- function(headers) {

  if (is.null(headers))
    return(FALSE)

  if (getRversion() >= "3.6.0")
    return(FALSE)

  envir <- parent.frame()
  renv_download_default_agent_scope_impl(headers, envir)

}

renv_download_default_agent_scope_impl <- function(headers, envir = NULL) {

  envir <- envir %||% parent.frame()

  utils <- asNamespace("utils")
  makeUserAgent <- utils$makeUserAgent

  ok <-
    is.function(makeUserAgent) &&
    identical(formals(makeUserAgent), pairlist(format = TRUE))

  if (!ok)
    return(FALSE)

  do.call("unlockBinding", list("makeUserAgent", utils))
  defer(do.call("lockBinding", list("makeUserAgent", utils)), envir = envir)

  agent <- makeUserAgent(FALSE)
  all <- c("User-Agent" = agent, headers)
  headertext <- paste0(names(all), ": ", all, "\r\n", collapse = "")

  assign("makeUserAgent", envir = utils, function(format = TRUE) {
    if (format) headertext else agent
  })

  return(TRUE)

}

renv_download_curl <- function(url, destfile, type, request, headers) {

  config <- renv_tempfile("renv-download-config-")

  fields <- c(
    "user-agent" = renv_http_useragent(),
    "url"        = url,
    "output"     = destfile
  )

  # set connect timeout
  timeout <- catch(as.integer(renv_config("connect.timeout", default = 20L)))
  if (is.numeric(timeout))
    fields[["connect-timeout"]] <- timeout

  # set number of retries
  retries <- catch(as.integer(renv_config("connect.retry", default = 3L)))
  if (is.numeric(retries))
    fields[["retry"]] <- retries

  # set up authentication headers
  auth <- renv_download_auth(url, type)
  if (length(auth)) {
    authtext <- paste(names(auth), auth, sep = ": ")
    names(authtext) <- "header"
    fields <- c(fields, authtext)
  }

  # add other custom headers
  if (length(headers)) {
    lines <- paste(names(headers), headers, sep = ": ")
    names(lines) <- "header"
    fields <- c(fields, lines)
  }

  # join together
  keys <- names(fields)
  vals <- shQuote(fields, type = "cmd")
  text <- paste(keys, vals, sep = " = ")

  # add in stand-along flags
  flags <- c("location", "fail", "silent", "show-error")
  if (request == "HEAD")
    flags <- c(flags, "head", "include")

  text <- c(flags, text)

  writeLines(text, con = config)

  args <- stack()

  extra <- getOption("download.file.extra")
  if (length(extra))
    args$push(extra)

  userconfig <- getOption(
    "renv.curl.config",
    renv_download_curl_config()
  )

  for (entry in userconfig)
    if (file.exists(entry))
      args$push("--config", shQuote(entry))

  args$push("--config", shQuote(config))

  stdout <- tempfile("renv-curl-stdout-")
  stderr <- tempfile("renv-curl-stderr-")

  status <- system2("curl", args$data(), stdout = stdout, stderr = stderr)

  if (file.exists(stderr)) {
    errs <- readLines(stderr)
    if (length(errs))
      warning(errs)
  }

  status

}

renv_download_curl_config <- function() {

  rc <- if (renv_platform_windows()) "_curlrc" else ".curlrc"

  homes <- c(
    Sys.getenv("CURL_HOME"),
    Sys.getenv("HOME"),
    Sys.getenv("R_USER"),
    path.expand("~/")
  )

  # nocov start
  if (renv_platform_windows()) {
    extra <- c(
      Sys.getenv("APPDATA"),
      file.path(Sys.getenv("USERPROFILE"), "Application Data"),
      dirname(Sys.which("curl"))
    )
    homes <- c(homes, extra)
  }
  # nocov end

  homes <- Filter(nzchar, homes)

  for (home in homes) {
    path <- file.path(home, rc)
    if (file.exists(path))
      return(path)
  }

  NULL

}

# nocov start

renv_download_wget <- function(url, destfile, type, request, headers) {

  config <- renv_tempfile("renv-download-config-")

  fields <- c(
    "user-agent" = renv_http_useragent(),
    "quiet"      = "on"
  )

  auth <- renv_download_auth(url, type)
  if (length(auth)) {
    authtext <- paste(names(auth), auth, sep = ": ")
    names(authtext) <- "header"
    fields <- c(fields, authtext)
  }

  if (length(headers)) {
    lines <- paste(names(headers), headers, sep = ": ")
    names(lines) <- "header"
    fields <- c(fields, lines)
  }

  keys <- names(fields)
  vals <- unlist(fields)
  text <- paste(keys, vals, sep = " = ")

  writeLines(text, con = config)

  args <- getOption("download.file.extra")

  args <- c(args, "--config", shQuote(config))
  if (request == "HEAD") {
    args <- c("--server-response", "--spider", args, shQuote(url))
    return(system2("wget", args, stdout = destfile, stderr = destfile))
  }

  args <- c(args, shQuote(url), "-O", shQuote(destfile))
  system2("wget", args)

}

# nocov end

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

  type <- tolower(type %||% renv_download_auth_type(url))
  switch(
    type,
    bitbucket = renv_download_auth_bitbucket(),
    github = renv_download_auth_github(),
    gitlab = renv_download_auth_gitlab(),
    character()
  )

}

renv_download_auth_bitbucket <- function() {

  user <-
    Sys.getenv("BITBUCKET_USER", unset = NA) %NA%
    Sys.getenv("BITBUCKET_USERNAME", unset = NA)

  pass <-
    Sys.getenv("BITBUCKET_PASS", unset = NA) %NA%
    Sys.getenv("BITBUCKET_PASSWORD", unset = NA)

  if (is.na(user) || is.na(pass))
    return(character())

  userpass <- paste(user, pass, sep = ":")
  c("Authorization" = paste("Basic", renv_base64_encode(userpass)))

}

renv_download_auth_github <- function() {

  pat <- Sys.getenv("GITHUB_PAT", unset = NA)
  if (is.na(pat))
    return(character())

  c("Authorization" = paste("token", pat))

}

renv_download_auth_gitlab <- function() {

  pat <- Sys.getenv("GITLAB_PAT", unset = NA)
  if (is.na(pat))
    return(character())

  c("Private-Token" = pat)

}

renv_download_headers <- function(url, type, headers) {

  # check for compatible download method
  method <- renv_download_file_method()
  if (!method %in% c("libcurl", "curl", "wget"))
    return(list())

  # perform the download
  file <- renv_tempfile("renv-headers-")
  renv_download_impl(
    url = url,
    destfile = file,
    type = type,
    request = "HEAD",
    headers = headers
  )

  if (!file.exists(file))
    return(list())

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

renv_download_size <- function(url, type, headers = NULL) {

  headers <- catch(renv_download_headers(url, type, headers))
  if (inherits(headers, "error"))
    return(-1L)

  size <- headers[["x-gitlab-size"]]
  if (!is.null(size))
    return(as.numeric(size))

  size <- headers[["content-length"]]
  if (!is.null(size))
    return(as.numeric(size))

  return(-1L)

}

# select an appropriate download file method. we prefer curl
# when available as it's the most user-customizable of all the
# download methods; when not available, we fall back to libcurl
# and wget (in that order). note that we don't want to use the
# internal or wininet downloaders as we cannot set custom headers
# with those methods. users can force a method with the
# RENV_DOWNLOAD_FILE_METHOD environment variable but we generally
# want to override a user-specified 'download.file.method'
renv_download_file_method <- function() {

  method <- Sys.getenv("RENV_DOWNLOAD_FILE_METHOD", unset = NA)
  if (!is.na(method))
    return(method)

  method <- getOption("download.file.method", default = "auto")
  if (method %in% c("curl", "wget"))
    return(method)

  if (nzchar(Sys.which("curl")))
    return("curl")

  libcurl <- capabilities("libcurl")
  if (length(libcurl) && libcurl)
    return("libcurl")

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

renv_download_check_archive <- function(destfile) {

  # validate the file exists
  if (!file.exists(destfile))
    return(FALSE)

  # validate archive type
  type <- renv_archive_type(destfile)
  if (type == "unknown")
    return(FALSE)

  # try listing files in the archive
  tryCatch({renv_archive_list(destfile); TRUE}, error = identity)

}
