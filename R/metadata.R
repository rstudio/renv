
renv_metadata_version <- function() {
  metadata <- get("_renv_metadata", envir = renv_envir_self())
  get("version", envir = metadata)
}

renv_metadata_embedded <- function() {
  metadata <- get("_renv_metadata", envir = renv_envir_self())
  get("embedded", envir = metadata)
}

renv_metadata_init <- function() {

  # only done for non-embedded renv
  if (exists("_renv_metadata", envir = renv_envir_self()))
    return()

  # set up metadata
  metadata <- list(
    embedded = FALSE,
    version  = renv_namespace_version("renv")
  )

  # create in namespace
  assign(
    x     = "_renv_metadata",
    value = as.environment(metadata),
    envir = renv_envir_self()
  )

}
