
# NOTE: 'the$metadata' is initialized either in 'renv_metadata_init()', for
# stand-alone installations of renv, or via an embedded initialize script for
# vendored copies of renv.

renv_metadata_create <- function(embedded, version, sha = NULL) {
  list(embedded = embedded, version = version, sha = sha)
}

renv_metadata_embedded <- function() {
  the$metadata$embedded
}

renv_metadata_version <- function() {
  the$metadata$version
}

renv_metadata_sha <- function() {
  the$metadata$sha
}

renv_metadata_remote <- function(metadata = the$metadata) {

  if (!is.null(metadata$sha))
    paste("rstudio/renv", metadata$sha, sep = "@")
  else
    paste("renv", metadata$version, sep = "@")

}

renv_metadata_version_friendly <- function(metadata = the$metadata) {

  version <- metadata$version

  sha <- metadata$sha
  if (!is.null(sha))
    version <- sprintf("%s; rstudio/renv@%s", version, substring(sha, 1L, 7L))

  version

}

renv_metadata_init <- function() {

  # if renv was embedded, then the$metadata should already be initialized
  if (!is.null(the$metadata))
    return()

  # renv doesn't appear to be embedded; initialize metadata based on the
  # currently-loaded version of renv
  the$metadata <- renv_metadata_create(
    embedded = FALSE,
    version  = renv_namespace_version("renv"),
    sha      = packageDescription("renv")[["RemoteSha"]]
  )

}
