
renv_metadata_version <- function() {
  the$metadata$sha %||% the$metadata$version
}

renv_metadata_version_friendly <- function(metadata = the$metadata) {
  ver <- metadata$version

  if (renv_metadata_is_dev(metadata)) {
    ver <- paste0(ver, "; rstudio/renv@", substr(metadata$sha, 1, 7))
  }

  ver
}

renv_metadata_is_dev <- function(metadata = the$metadata) {
  if (!is.null(metadata$sha)) {
    TRUE
  } else {
    renv_version_length(metadata$version) != 3L
  }
}

renv_metadata_embedded <- function() {
  the$metadata$embedded
}

renv_metadata_init <- function() {

  # only done for non-embedded renv
  if (exists("metadata", envir = the))
    return()

  the$metadata <- list(
    embedded = FALSE,
    version  = renv_namespace_version("renv"),
    sha = packageDescription("renv")[["RemoteSha"]]
  )

}
