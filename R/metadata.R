
# NOTE: 'the$metadata' is initialized either in 'renv_metadata_init()', for
# stand-alone installations of renv, or via an embedded initialize script for
# vendored copies of renv.

renv_metadata_create <- function(embedded, version) {
  list(embedded = embedded, version = version)
}

renv_metadata_embedded <- function() {
  the$metadata$embedded
}

renv_metadata_version <- function() {
  the$metadata$version
}


renv_metadata_remote <- function(metadata = the$metadata) {

  # check for development versions
  sha <- attr(metadata$version, "sha")
  if (!is.null(sha) && nzchar(sha))
    return(paste("rstudio/renv", sha, sep = "@"))

  # otherwise, use release version
  paste("renv", metadata$version, sep = "@")

}

renv_metadata_version_friendly <- function(metadata = the$metadata) {
  renv_bootstrap_version_friendly(metadata$version)
}

renv_metadata_init <- function() {

  # if renv was embedded, then the$metadata should already be initialized
  if (!is.null(the$metadata))
    return()

  # renv doesn't appear to be embedded; initialize metadata based on the
  # currently-loaded version of renv
  version <- renv_namespace_version("renv")
  attr(version, "sha") <- packageDescription("renv")[["RemoteSha"]]
  the$metadata <- renv_metadata_create(embedded = FALSE, version = version)

}
