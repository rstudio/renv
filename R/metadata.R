
renv_metadata_version <- function() {
  `_renv_metadata`$sha %||% `_renv_metadata`$version
}

renv_metadata_version_friendly <- function(metadata = `_renv_metadata`) {
  ver <- metadata$version

  if (renv_metadata_is_dev(metadata)) {
    ver <- paste0(ver, "; rstudio/renv@", substr(metadata$sha, 1, 7))
  }

  ver
}

renv_metadata_is_dev <- function(metadata = `_renv_metadata`) {
  if (!is.null(metadata$sha)) {
    TRUE
  } else {
    renv_version_length(metadata$version) != 3L
  }
}

renv_metadata_embedded <- function() {
  `_renv_metadata`[["embedded"]]
}

renv_metadata_init <- function() {

  # only done for non-embedded renv
  if (exists("_renv_metadata", envir = renv_envir_self()))
    return()

  # set up metadata
  metadata <- list(
    embedded = FALSE,
    version  = renv_namespace_version("renv"),
    sha = packageDescription("renv")[["RemoteSha"]]
  )

  # create in namespace
  assign(
    x     = "_renv_metadata",
    value = as.environment(metadata),
    envir = renv_envir_self()
  )

}
