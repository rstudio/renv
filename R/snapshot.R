#' Snapshot a Virtual Environment
#'
#' Snapshot a virtual environment, creating a manifest file that can be used
#' to later restore the virtual environment.
#'
#' @inheritParams renv-params
#'
#' @param file The location where the library manifest should be written.
#'   If `NULL`, the manifest (as an \R list, with each entry representing
#'   an installed package in the library) is returned instead.
#'
#' @family reproducibility
#'
#' @export
renv_snapshot <- function(renv = NULL, file = NULL) {
  renv <- renv_active_renv(renv)

  config <- renv_config_read(renv_paths_config(renv))
  library <- renv_snapshot_library(renv)
  manifest <- list(config = config, library = library)
  if (is.null(file))
    return(manifest)

  renv_manifest_write(manifest, file = file)
}

renv_snapshot_library <- function(renv) {

  config <- renv_config_read(renv_paths_config(renv))
  libs <- config$r_libs
  lapply(libs, renv_snapshot_library_one)

}

renv_snapshot_library_one <- function(library) {

  path <- renv_paths_library(library)
  if (!file.exists(path))
    stopf("Library '%s' does not exist.", library)

  pkgs <- list.files(path, full.names = TRUE)
  descriptions <- file.path(pkgs, "DESCRIPTION")
  packages <- lapply(descriptions, renv_snapshot_description, library = library)

  broken <- Filter(function(record) inherits(record, "error"), packages)
  if (length(broken)) {
    messages <- vapply(broken, conditionMessage, character(1))
    header <- sprintf("Error(s) snapshotting library '%s':", library)
    body <- paste("-", messages, collapse = "\n")
    message <- paste(header, body, sep = "\n")
    stop(message, call. = FALSE)
  }

  names(packages) <- vapply(packages, `[[`, "Package", FUN.VALUE = character(1))
  packages

}

renv_snapshot_description <- function(path, library) {

  if (!file.exists(path)) {
    fmt <- "No DESCRIPTION at path '%s'"
    msg <- sprintf(fmt, path)
    return(simpleError(msg))
  }

  # TODO: Check for tempfiles that sneak into library path, e.g. 'file<abcd>'
  # Report and skip?
  dcf <- tryCatch(read.dcf(path, all = TRUE), error = identity)
  if (inherits(dcf, "error"))
    return(dcf)

  if (is.null(dcf[["Repository"]]))
    dcf[["Repository"]] <- "<unknown>"

  dcf[["Library"]] <- library

  fields <- c("Package", "Version", "Repository", "Library")
  missing <- setdiff(fields, names(dcf))
  if (length(missing)) {
    fmt <- "Required fields %s missing from DESCRIPTION at path '%s'"
    msg <- sprintf(fmt, paste(shQuote(missing), collapse = ", "), path)
    return(simpleError(msg))
  }

  fields <- c(fields, grep("^Remote", names(dcf)))
  dcf[fields]

}

