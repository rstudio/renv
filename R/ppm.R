
renv_ppm_normalize <- function(url) {
  sub("/__[^_]+__/[^/]+/", "/", url)
}

renv_ppm_transform <- function(repos = getOption("repos")) {
  map_chr(repos, function(url) {
    tryCatch(
      renv_ppm_transform_impl(url),
      error = function(e) url
    )
  })
}

renv_ppm_transform_impl <- function(url) {

  # repository URL transformation is only necessary on Linux
  os <- renv_ppm_os()
  if (!identical(os, "__linux__"))
    return(url)

  # check for a known platform
  platform <- renv_ppm_platform()
  if (is.null(platform))
    return(url)

  # don't transform non-https URLs
  if (!grepl("^https?://", url))
    return(url)

  # if this already appears to be a binary URL, then avoid
  # transforming it
  if (grepl("/__[^_]+__/", url))
    return(url)

  # only attempt to transform URLs that are formatted like PPM urls:
  #
  #   https://ppm.company.org/cran/checkpoint/id
  #
  # in particular, there should be at least two trailing
  # alphanumeric path components
  pattern <- "/[^/]+/[^/]+/"
  if (!grepl(pattern, url))
    return(url)

  # begin building list of URLs
  urls <- character()

  # ignore some known CRAN mirrors
  # (note that getCRANmirrors can fail if R is not installed with
  # any local knowledge of available CRAN repositories)
  mirrors <- catch(getCRANmirrors(local.only = TRUE))
  urls <- c(urls, mirrors$URL)

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

  # if this is a 'known' PPM instance, then skip the query step
  known <- c(
    dirname(dirname(config$ppm.url())),
    getOption("renv.ppm.repos", default = NULL)
  )

  if (any(startswith(url, known))) {
    parts <- c(dirname(url), "__linux__", platform, basename(url))
    binurl <- paste(parts, collapse = "/")
    return(binurl)
  }

  # try to query the status endpoint
  # TODO: this could fail if the URL is a proxy back to PPM?
  base <- dirname(dirname(url))
  status <- catch(renv_ppm_status(base))
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

renv_ppm_status <- function(base) {
  memoize(
    key   = base,
    value = catch(renv_ppm_status_impl(base))
  )
}

renv_ppm_status_impl <- function(base) {

  # use a shorter delay to avoid hanging a session
  renv_scope_options(
    renv.config.connect.timeout = 10L,
    renv.config.connect.retry   = 1L
  )

  # attempt the download
  endpoint <- file.path(base, "__api__/status")
  destfile <- renv_scope_tempfile("renv-ppm-status-", fileext = ".json")
  quietly(download(endpoint, destfile))

  # read the downloaded JSON
  renv_json_read(destfile)

}

renv_ppm_platform <- function(file = "/etc/os-release") {

  platform <- Sys.getenv("RENV_PPM_PLATFORM", unset = NA)
  if (!is.na(platform))
    return(platform)

  platform <- Sys.getenv("RENV_RSPM_PLATFORM", unset = NA)
  if (!is.na(platform))
    return(platform)

  if (renv_platform_windows())
    return("windows")

  if (renv_platform_macos())
    return("macos")

  renv_ppm_platform_impl(file)

}

renv_ppm_platform_impl <- function(file = "/etc/os-release") {

  if (file.exists(file)) {

    properties <- renv_properties_read(
      path      = file,
      delimiter = "=",
      dequote   = TRUE
    )

    id <- properties$ID %||% ""

    case(
      identical(id, "ubuntu") ~ renv_ppm_platform_ubuntu(properties),
      identical(id, "centos") ~ renv_ppm_platform_centos(properties),
      identical(id, "rhel")   ~ renv_ppm_platform_rhel(properties),
      grepl("\\bsuse\\b", id) ~ renv_ppm_platform_suse(properties)
    )

  }

}

renv_ppm_platform_ubuntu <- function(properties) {

  codename <- properties$VERSION_CODENAME
  if (is.null(codename))
    return(NULL)

  codename

}

renv_ppm_platform_centos <- function(properties) {

  id <- properties$VERSION_ID
  if (is.null(id))
    return(NULL)

  paste0("centos", substring(id, 1L, 1L))

}

renv_ppm_platform_rhel <- function(properties) {

  id <- properties$VERSION_ID
  if (is.null(id))
    return(NULL)

  paste0("centos", substring(id, 1L, 1L))

}


renv_ppm_platform_suse <- function(properties) {

  id <- properties$VERSION_ID
  if (is.null(id))
    return(NULL)

  parts <- strsplit(id, ".", fixed = TRUE)[[1L]]
  paste0("opensuse", parts[[1L]])

}

renv_ppm_os <- function() {

  os <- Sys.getenv("RENV_PPM_OS", unset = NA)
  if (!is.na(os))
    return(os)

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


renv_ppm_enabled <- function() {

  # allow environment variable override
  enabled <- Sys.getenv("RENV_PPM_ENABLED", unset = NA)
  if (!is.na(enabled))
    return(truthy(enabled, default = TRUE))

  enabled <- Sys.getenv("RENV_RSPM_ENABLED", unset = NA)
  if (!is.na(enabled))
    return(truthy(enabled, default = TRUE))

  # TODO: can we remove this check?
  # https://github.com/rstudio/renv/issues/1132
  disabled <-
    renv_platform_linux() &&
    identical(renv_platform_machine(), "aarch64")

  if (disabled)
    return(FALSE)

  # otherwise, use configuration option
  config$ppm.enabled()

}
