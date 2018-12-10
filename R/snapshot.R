
#' Snapshot a Virtual Environment
#'
#' Snapshot a virtual environment, creating a manifest file that can be used
#' to later restore the virtual environment.
#'
#' @inheritParams renv-params
#'
#' @param file The location where the library manifest should be written. If
#'   `NULL`, the manifest is returned as an \R object. If `""` (the empty
#'   string), a project-local manifest is written to file.
#'
#' @family reproducibility
#'
#' @export
renv_snapshot <- function(name = NULL,
                          file = "",
                          confirm = interactive())
{
  name <- renv_active_renv(name)

  config <- renv_config_read(renv_paths_config(name))
  library <- renv_snapshot_library(name)
  new <- list(config = config, library = library)

  if (identical(file, ""))
    file <- file.path(renv_active_project(), "renv/manifest")

  if (is.null(file))
    return(new)

  path <- file.path(renv_active_project(), "renv/manifest")
  old <- list(config = list(), library = list())
  if (file.exists(path))
    old <- renv_manifest_read(path)

  actions <- renv_manifest_diff(old, new)
  if (empty(actions)) {
    message("* The manifest is already up-to-date.")
    return(invisible(new))
  }

  if (confirm || renv_verbose()) {
    renv_snapshot_report_actions(actions, old, new)
    messagef("* The manifest will be written to '%s'.", file)
  }

  if (confirm) {
    response <- readline("Do you want to proceed? [Y/n]: ")
    if (response != "y") {
      message("Operation aborted.")
      return(invisible(new))
    }
  }

  renv_manifest_write(new, file = file)
  messagef("* Manifest written to '%s'.", aliased_path(file))

  invisible(new)
}

renv_snapshot_library <- function(name) {
  # TODO: do we ever want to include the user library, or system library,
  # in the lockfile?
  config <- renv_config_read(renv_paths_config(name))
  uapply(config$r_libs, renv_snapshot_library_one)
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
    messages <- map_chr(broken, conditionMessage)
    header <- sprintf("Error(s) snapshotting library '%s':", library)
    body <- paste("-", messages, collapse = "\n")
    message <- paste(header, body, sep = "\n")
    stop(message, call. = FALSE)
  }

  names(packages) <- map_chr(packages, `[[`, "Package")
  packages

}

renv_snapshot_description <- function(path, library) {

  if (!file.exists(path)) {
    fmt <- "No DESCRIPTION at path '%s'."
    msg <- sprintf(fmt, path)
    return(simpleError(msg))
  }

  # TODO: Check for tempfiles that sneak into library path, e.g. 'file<abcd>'
  # Report and skip?
  dcf <- tryCatch(read.dcf(path, all = TRUE), error = identity)
  if (inherits(dcf, "error"))
    return(dcf)

  dcf[["Library"]] <- library
  # TODO: Mark e.g. GitHub sources and friends
  dcf[["Source"]] <- dcf[["Repository"]] %||% "<unknown>"

  fields <- c("Package", "Version", "Library", "Source")
  missing <- setdiff(fields, names(dcf))
  if (length(missing)) {
    fmt <- "Required fields %s missing from DESCRIPTION at path '%s'."
    msg <- sprintf(fmt, paste(shQuote(missing), collapse = ", "), path)
    return(simpleError(msg))
  }

  fields <- c(fields, grep("^Remote", names(dcf)))
  as.list(dcf[fields])

}

renv_snapshot_report_actions <- function(actions, old, new) {

  if (!renv_verbose())
    return(invisible())

  if ("install" %in% actions) {
    msg <- "The following package(s) will be added to the manifest:"
    renv_pretty_print(msg, new, actions, "install")
  }

  if ("remove" %in% actions) {
    msg <- "The following package(s) will be removed from the manifest:"
    renv_pretty_print(msg, old, actions, "remove")
  }

  if ("upgrade" %in% actions) {
    msg <- "The following package(s) will be upgraded in the manifest:"
    renv_pretty_print_pair(msg, old, new, actions, "upgrade")
  }

  if ("downgrade" %in% actions) {
    msg <- "The following package(s) will be downgraded in the manifest:"
    renv_pretty_print_pair(msg, old, new, actions, "downgrade")
  }

  if ("crossgrade" %in% actions) {
    msg <- "The following package(s) will be modified in the manifest:"
    renv_pretty_print_pair(msg, old, new, actions, "crossgrade")
  }

}
