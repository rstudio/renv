#' @inheritParams renv-params
#'
#' @param file The location where the library manifest should be written.
#'   If `NULL`, the manifest (as an \R list, with each entry representing
#'   an installed package in the library) is returned instead.
#'
#' @export
renv_snapshot <- function(renv = NULL, local = FALSE, file = NULL) {
  renv <- renv_active_renv(renv)

  libpaths <- renv_snapshot_libpaths(renv)
  config <- renv_snapshot_config(renv)
  manifest <- list(config = config, libpaths = libpaths)
  if (is.null(file))
    return(manifest)

  renv_manifest_write(manifest, file = file)
}

renv_sanpshot_libpaths <- function(renv) {

  config <- renv_config_read(renv)
  path <- renv_paths_library(library, local = local)
  if (!file.exists(path))
    stopf("Library '%s' does not exist.", library)

  pkgs <- list.files(path, full.names = TRUE)
  descriptions <- file.path(pkgs, "DESCRIPTION")
  packages <- lapply(descriptions, renv_manifest_create_entry)

  broken <- Filter(function(record) inherits(record, "error"), packages)
  if (length(broken)) {
    messages <- vapply(broken, conditionMessage, character(1))
    header <- sprintf("Error(s) snapshotting library '%s':", library)
    body <- paste("-", messages, collapse = "\n")
    message <- paste(header, body, sep = "\n")
    stop(message, call. = FALSE)
  }

  configuration <- renv_config_read()

  if (is.null(file))
    return(packages)

  renv_manifest_write(packages, file = file)

}

