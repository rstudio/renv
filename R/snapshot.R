
#' Snapshot a Virtual Environment
#'
#' Snapshot a virtual environment, creating a **manifest** file that can be used
#' to later restore the virtual environment. See the [manifest] documentation
#' for more details on the structure of a manifest.
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
snapshot <- function(name = NULL, file = "", confirm = interactive()) {

  name <- name %||% renv_state$environment()
  if (!nzchar(name)) {
    msg <- paste(
      "This project has no active virtual environment.",
      "Have you called `renv::activate()` yet?"
    )
    stop(msg)
  }

  # generate a new manifest
  new <- renv_manifest_read(renv_paths_environment(name))

  # update state-related fields
  if (file.exists(renv_activate_path()))
    new$Environment <- renv_activate_read()

  new$R$Package <- uapply(new$R$Library, function(library) {
    renv_snapshot_r_library(library, renv_paths_library(library))
  })

  # return it directly when 'file' is NULL
  if (is.null(file))
    return(new)

  # interpret empty file path
  if (!nzchar(file) || !length(file))
    file <- renv_snapshot_manifest_path()

  # attempt to read the old manifest (if it exists)
  old <- if (nzchar(renv_active_manifest()))
    renv_manifest_read(renv_active_manifest())
  else
    list()

  # diff manifest packages to get set of actions
  diff <- renv_manifest_diff(old, new)
  if (empty(diff)) {
    vmessagef("* The manifest is already up-to-date.")
    return(invisible(new))
  }

  # check for missing dependencies and warn if any are discovered
  if (!renv_snapshot_validate_dependencies(new, confirm))
    return(invisible(new))

  # report actions to the user
  actions <- renv_manifest_diff_packages(old, new)
  if (confirm && renv_verbose()) {
    renv_snapshot_report_actions(actions, old, new)
    printf("The manifest will be written to '%s'.", aliased_path(file))
  }

  # request user confirmation
  if (confirm) {
    response <- readline("Do you want to proceed? [Y/n]: ")
    if (response != "y") {
      message("Operation aborted.")
      return(invisible(new))
    }
  }

  # write it out
  ensure_parent_directory(file)
  renv_manifest_write(new, file = file)
  vmessagef("* Manifest written to '%s'.", aliased_path(file))

  invisible(new)

}

renv_snapshot_validate_dependencies <- function(manifest, confirm) {

  packages <- manifest$R$Package

  installed <- as.data.frame(installed.packages(), stringsAsFactors = FALSE)
  missing <- lapply(packages, function(package) {
    path <- renv_paths_library(package$Library, package$Package)
    deps <- renv_dependencies_discover_description(path)
    setdiff(deps$Package, c("R", installed$Package))
  })

  bad <- Filter(length, missing)
  if (!length(bad))
    return(TRUE)

  if (confirm) {

    text <- lines(
      "The following package(s) depend on packages which are not currently installed:",
      "",
      sprintf("\t%s: %s", names(bad), map_chr(bad, toString)),
      "",
      "Consider re-installing these packages before snapshotting the manifest."
    )
    message(text)

    response <- readline("Do you want to proceed? [Y/n]: ")
    if (!identical(tolower(response), "y")) {
      messagef("Operation aborted.")
      return(FALSE)
    }
  }

  TRUE

}

renv_snapshot_r_library <- function(name, library, synchronize = TRUE) {

  pkgs <- list.files(library, full.names = TRUE)
  pkgs <- renv_snapshot_r_library_diagnose(library, pkgs)

  descriptions <- file.path(pkgs, "DESCRIPTION")
  records <- lapply(descriptions, renv_snapshot_description, name)

  broken <- Filter(function(record) inherits(record, "error"), records)
  if (length(broken)) {
    messages <- map_chr(broken, conditionMessage)
    header <- sprintf("Error(s) snapshotting library '%s':", library)
    body <- paste("-", messages, collapse = "\n")
    message <- paste(header, body, sep = "\n")
    stop(message, call. = FALSE)
  }

  if (synchronize)
    lapply(records, renv_cache_synchronize)

  names(records) <- map_chr(records, `[[`, "Package")
  records

}

renv_snapshot_r_library_diagnose <- function(library, pkgs) {

  pkgs <- renv_snapshot_r_library_diagnose_tempfile(library, pkgs)
  pkgs <- renv_snapshot_r_library_diagnose_missing_description(library, pkgs)
  pkgs

}

renv_snapshot_r_library_diagnose_tempfile <- function(library, pkgs) {

  names <- basename(pkgs)
  missing <- grepl("^file(?:\\w){12}", names)
  if (!any(missing))
    return(pkgs)

  fmt <- lines(
    "The following folder(s) in library '%s' appear to be left-over temporary directories:",
    "",
    paste("-", basename(pkgs)[missing], collapse = "\n"),
    "",
    "Consider removing these folders from your library."
  )

  warningf(fmt, library, immediate. = TRUE)
  pkgs[!missing]

}

renv_snapshot_r_library_diagnose_missing_description <- function(library, pkgs) {

  desc <- file.path(pkgs, "DESCRIPTION")
  missing <- !file.exists(desc)
  if (!any(missing))
    return(pkgs)

  fmt <- lines(
    "The following package(s) in library '%s' are missing DESCRIPTION files:",
    "",
    paste("-", basename(pkgs)[missing], collapse = "\n"),
    "",
    "Consider removing or re-installing these packages."
  )

  warningf(fmt, library, immediate. = TRUE)
  pkgs[!missing]

}

renv_snapshot_description <- function(path, library) {

  info <- file.info(path)
  if (identical(info$isdir, TRUE))
    path <- file.path(path, "DESCRIPTION")

  if (!file.exists(path)) {
    fmt <- "No DESCRIPTION at path '%s'."
    msg <- sprintf(fmt, path)
    return(simpleError(msg))
  }

  # TODO: Check for tempfiles that sneak into library path, e.g. 'file<abcd>'
  # Report and skip?
  dcf <- catch(renv_dcf_read(path))
  if (inherits(dcf, "error"))
    return(dcf)

  dcf[["Library"]] <- library
  dcf[["Source"]] <- renv_snapshot_description_source(dcf)
  dcf[["Hash"]] <- renv_hash_description(path)

  fields <- c("Package", "Version", "Library", "Source", "Hash")
  missing <- setdiff(fields, names(dcf))
  if (length(missing)) {
    fmt <- "Required fields %s missing from DESCRIPTION at path '%s'."
    msg <- sprintf(fmt, paste(shQuote(missing), collapse = ", "), path)
    return(simpleError(msg))
  }

  fields <- c(fields, grep("^Remote", names(dcf), value = TRUE))
  as.list(dcf[fields])

}

renv_snapshot_description_source <- function(dcf) {

  # TODO: record repository name explicitly?
  if (!is.null(dcf[["Repository"]]))
    return("CRAN")

  remote <- dcf[["RemoteType"]] %||% "unknown"
  renv_alias(remote)

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

# NOTE: would like to use ISO 8601 timestamps but ':' is not supported
# in Windows filenames
renv_snapshot_manifest_path <- function(project = NULL) {
  project <- project %||% renv_state$project()
  time <- Sys.time()
  ymd <- strftime(time, "%Y-%m-%d")
  timestamp <- strftime(time, "%Y-%m-%dT%H-%M-%S%Z")
  file.path(project, "renv/manifest", ymd, timestamp)
}
