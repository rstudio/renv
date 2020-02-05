
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

  # check that we have a known OS, platform
  os <- renv_rspm_os()
  if (is.null(os))
    return(url)

  platform <- renv_rspm_platform()
  if (is.null(platform))
    return(url)

  # if this is already a binary URL, then nothing to do
  if (grepl("/__[^_]+__/", url))
    return(url)

  # check if this is truly an RSPM url
  if (!grepl("/cran/", url))
    return(url)

  # try to query the status endpoint
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
      parts <- c(base, "cran", os, platform, basename(url))
      binurl <- paste(parts, collapse = "/")
      return(binurl)
    }

  }

  # no match; return url as-is
  url

}

renv_rspm_status <- function(base) {

  status <- `_renv_rspm_status`[[base]]
  if (!is.null(status))
    return(status)

  endpoint <- file.path(base, "__api__/status")
  destfile <- renv_tempfile("renv-rspm-status-", fileext = ".json")
  quietly(download(endpoint, destfile))
  status <- renv_json_read(destfile)

  `_renv_rspm_status`[[base]] <- status
  status

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

renv_rspm_platform <- function() {

  platform <- Sys.getenv("RENV_RSPM_PLATFORM", unset = NA)
  if (!is.na(platform))
    return(platform)

  if (renv_platform_windows())
    return("windows")

  if (renv_platform_macos())
    return("macos")

  if (file.exists("/etc/os-release")) {

    properties <- renv_read_properties("/etc/os-release", delimiter = "=")

    id <- properties$ID %||% ""
    id_like <- properties$ID_LIKE %||% ""
    version_codename <- properties$VERSION_CODENAME %||% ""
    version_id <- properties$VERSION_ID %||% ""

    if (id == "ubuntu")
      return(version_codename)

    if (id == "centos")
      return(paste(id, version_id, sep = ""))

    if (grepl("\\bsuse\\b", id_like)) {
      parts <- strsplit(version_id, ".", fixed = TRUE)
      return(paste("opensuse", parts[[1]], sep = ""))
    }

  }

}

renv_rspm_enabled <- function() {
  renv_config("rspm.enabled", default = TRUE)
}
