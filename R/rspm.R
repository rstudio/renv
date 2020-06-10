
`_renv_rspm_status` <- new.env(parent = emptyenv())

renv_rspm_transform <- function(repos) {
  map_chr(repos, function(url) {
    tryCatch(
      renv_rspm_transform_impl(url),
      error = function(e) url
    )
  })
}

renv_rspm_transform_impl <- function(url) {

  # repository URL transformation is only necessary on Linux
  os <- renv_rspm_os()
  if (!identical(os, "__linux__"))
    return(url)

  # check for a known platform
  platform <- renv_rspm_platform()
  if (is.null(platform))
    return(url)

  # don't transform non-https URLs
  if (!grepl("^https?://", url))
    return(url)

  # if this already appears to be a binary URL, then avoid
  # transforming it
  if (grepl("/__[^_]+__/", url))
    return(url)

  # only attempt to transform URLs that are formatted like
  # RSPM urls -- for example:
  #
  #   https://rspm.company.org/cran/checkpoint/id
  #
  # in particular, there should be at least two trailing
  # alphanumeric path components
  pattern <- "/[^/]+/[^/]+/*$"
  if (!grepl(pattern, url))
    return(url)

  # ignore some known CRAN mirrors
  mirrors <- getCRANmirrors(local.only = TRUE)
  urls <- mirrors$URL

  # also ignore some RStudio URLs
  rstudio <- c(
    "http://cran.rstudio.com",
    "http://cran.rstudio.org",
    "https://cran.rstudio.com",
    "https://cran.rstudio.org"
  )
  urls <- c(urls, rstudio)

  if (sub("/+$", "", url) %in% sub("/+$", "", urls))
    return(url)

  # try to query the status endpoint
  # TODO: this could fail if the URL is a proxy back to RSPM
  base <- dirname(dirname(url))
  status <- catch(renv_rspm_status(base))
  if (inherits(status, "error"))
    return(url)

  # iterate through distros and check for a match
  for (distro in status$distros) {

    ok <-
      identical(distro$binaryURL, platform) &&
      identical(distro$binaries, TRUE)

    if (ok) {
      parts <- c(dirname(url), "__linux__", platform, basename(url))
      binurl <- paste(parts, collapse = "/")
      return(binurl)
    }

  }

  # no match; return url as-is
  url

}

renv_rspm_status <- function(base) {
  memoize(
    key   = base,
    expr  = catch(renv_rspm_status_impl(base)),
    envir = `_renv_rspm_status`
  )
}

renv_rspm_status_impl <- function(base) {

  # use a shorter delay to avoid hanging a session
  renv_scope_options(
    renv.config.connect.timeout = 10L,
    renv.config.connect.retry   = 1L
  )

  # attempt the download
  endpoint <- file.path(base, "__api__/status")
  destfile <- renv_tempfile("renv-rspm-status-", fileext = ".json")
  quietly(download(endpoint, destfile))

  # read the downloaded JSON
  renv_json_read(destfile)

}

renv_rspm_platform <- function() {

  platform <- Sys.getenv("RENV_RSPM_PLATFORM", unset = NA)
  if (!is.na(platform))
    return(platform)

  if (renv_platform_windows())
    return("windows")

  if (renv_platform_macos())
    return("macos")

  if (file.exists("/etc/os-release")) {

    properties <- renv_properties_read(
      path      = "/etc/os-release",
      delimiter = "=",
      dequote   = TRUE
    )

    id <- properties$ID %||% ""

    case(
      identical(id, "ubuntu") ~ renv_rspm_platform_ubuntu(properties),
      identical(id, "centos") ~ renv_rspm_platform_centos(properties),
      grepl("\\bsuse\\b", id) ~ renv_rspm_platform_suse(properties)
    )

  }

}

renv_rspm_platform_ubuntu <- function(properties) {

  codename <- properties$VERSION_CODENAME
  if (is.null(codename))
    return(NULL)

  codename

}

renv_rspm_platform_centos <- function(properties) {

  id <- properties$VERSION_ID
  if (is.null(id))
    return(NULL)

  paste0("centos", id)

}

renv_rspm_platform_suse <- function(properties) {

  id <- properties$VERSION_ID
  if (is.null(id))
    return(NULL)

  parts <- strsplit(id, ".", fixed = TRUE)[[1L]]
  paste0("opensuse", parts[[1L]])

}

renv_rspm_os <- function() {

  os <- Sys.getenv("RENV_RSPM_OS", unset = NA)
  if (!is.na(os))
    return(os)

  if (renv_platform_windows())
    "__windows__"
  else if (renv_platform_macos())
    "__macos__"
  else if (renv_platform_linux())
    "__linux__"

}


renv_rspm_enabled <- function() {
  config$rspm.enabled()
}
