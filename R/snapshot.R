
#' Snapshot a Project
#'
#' Call `snapshot()` to create a **lockfile** capturing the state of a project's
#' \R package dependencies. The lockfile can be used to later restore these
#' project's dependencies as required. See the [lockfile] documentation for more
#' details on the structure of a lockfile.
#'
#' In particular, `snapshot()` captures all \R packages currently installed
#' within the project's private library. Dependencies within any other libraries
#' (currently active on the library paths or not) are not captured.
#'
#' @inheritParams renv-params
#'
#' @param file The location where the generated lockfile should be written.
#'   When `NULL`, the lockfile (as an \R object) is returned directly instead.
#'
#' @family reproducibility
#'
#' @export
snapshot <- function(project = NULL,
                     file = file.path(project, "renv.lock"),
                     confirm = interactive())
{
  project <- project %||% renv_project()
  library <- renv_paths_library(project)

  new <- renv_lockfile_init()
  new$R$Package <- renv_snapshot_r_packages(library)

  if (is.null(file))
    return(new)

  on.exit(renv_python_snapshot(project), add = TRUE)

  old <- list()
  if (file.exists(file)) {
    old <- renv_lockfile_read(file)
    diff <- renv_lockfile_diff(old, new)
    if (empty(diff)) {
      vmessagef("* The lockfile is already up to date.")
      return(invisible(new))
    }
  }

  # check for missing dependencies and warn if any are discovered
  if (!renv_snapshot_validate(new, confirm))
    return(invisible(new))

  # report actions to the user
  actions <- renv_lockfile_diff_packages(old, new)
  if (confirm && renv_verbose()) {
    renv_snapshot_report_actions(actions, old, new)
    printf("The lockfile will be written to '%s'.", aliased_path(file))
  }

  # request user confirmation
  if (confirm && !proceed()) {
    message("Operation aborted.")
    return(invisible(new))
  }

  # write it out
  ensure_parent_directory(file)
  renv_lockfile_write(new, file = file)
  vmessagef("* Lockfile written to '%s'.", aliased_path(file))

  invisible(new)

}

renv_snapshot_validate <- function(lockfile, confirm) {
  renv_snapshot_validate_dependencies(lockfile, confirm) &&
  renv_snapshot_validate_sources(lockfile, confirm)
}

renv_snapshot_validate_dependencies <- function(lockfile, confirm) {

  records <- renv_records(lockfile)
  installed <- as.data.frame(installed.packages(), stringsAsFactors = FALSE)
  missing <- lapply(records, function(record) {
    path <- renv_paths_library(record$Library, record$Package)
    deps <- renv_dependencies_discover_description(path)
    setdiff(deps$Package, c("R", installed$Package))
  })

  bad <- Filter(length, missing)
  if (!length(bad))
    return(TRUE)

  renv_pretty_print_packages(
    names(bad),
    "The following package(s) depend on packages which are not currently installed:",
    "Consider re-installing these packages before snapshotting the lockfile.",
  )

  if (confirm && !proceed()) {
    message("Operation aborted.")
    return(FALSE)
  }

  TRUE

}

renv_snapshot_validate_sources <- function(lockfile, confirm) {

  records <- renv_records(lockfile)
  unknown <- Filter(
    function(record) (record$Source %||% "") == "unknown",
    records
  )

  if (empty(unknown))
    return(TRUE)

  renv_pretty_print_packages(
    names(unknown),
    "The following package(s) were installed from an unknown source:",
    "Consider re-installing these packages from a known source (e.g. CRAN)."
  )

  if (confirm && !proceed()) {
    message("Operation aborted.")
    return(FALSE)
  }

  TRUE

}

renv_snapshot_r_packages <- function(library = NULL) {

  # list packages in the library
  library <- library %||% renv_libpaths_default()
  paths <- list.files(library, full.names = TRUE)

  # remove 'base' packages
  ip <- renv_installed_packages_base()
  paths <- paths[!basename(paths) %in% c(ip$Package, "translations")]

  # remove ignored packages
  paths <- paths[!basename(paths) %in% c("renv", settings$ignored.packages())]

  # validate the remaining set of packages
  valid <- renv_snapshot_r_library_diagnose(library, paths)

  # remove duplicates (so only first package entry discovered in library wins)
  duplicated <- duplicated(basename(valid))
  packages <- valid[!duplicated]

  # snapshot description files
  descriptions <- file.path(packages, "DESCRIPTION")
  records <- lapply(descriptions, renv_snapshot_description)

  # report any snapshot failures
  broken <- Filter(function(record) inherits(record, "error"), records)
  if (length(broken)) {
    messages <- map_chr(broken, conditionMessage)
    header <- sprintf("Error(s) snapshotting library '%s':", library)
    body <- paste("-", messages, collapse = "\n")
    message <- paste(header, body, sep = "\n")
    stop(message, call. = FALSE)
  }

  # name results and return
  names(records) <- map_chr(records, `[[`, "Package")
  records

}

renv_snapshot_r_library_diagnose <- function(library, pkgs) {

  pkgs <- renv_snapshot_r_library_diagnose_broken_link(library, pkgs)
  pkgs <- renv_snapshot_r_library_diagnose_tempfile(library, pkgs)
  pkgs <- renv_snapshot_r_library_diagnose_missing_description(library, pkgs)
  pkgs

}

renv_snapshot_r_library_diagnose_broken_link <- function(library, pkgs) {

  broken <- !file.exists(pkgs)
  if (!any(broken))
    return(pkgs)

  renv_pretty_print_packages(
    basename(pkgs)[broken],
    "The following package(s) have broken symlinks into the cache:",
    "Consider re-installing these packages."
  )

  pkgs[!broken]

}

renv_snapshot_r_library_diagnose_tempfile <- function(library, pkgs) {

  names <- basename(pkgs)
  missing <- grepl("^file(?:\\w){12}", names)
  if (!any(missing))
    return(pkgs)

  renv_pretty_print_packages(
    basename(pkgs)[missing],
    "The following folder(s) appear to be left-over temporary directories:",
    "Consider removing these folders from your library."
  )

  pkgs[!missing]

}

renv_snapshot_r_library_diagnose_missing_description <- function(library, pkgs) {

  desc <- file.path(pkgs, "DESCRIPTION")
  missing <- !renv_file_exists(desc)
  if (!any(missing))
    return(pkgs)

  renv_pretty_print_packages(
    basename(pkgs[missing]),
    "The following package(s) are missing DESCRIPTION files:",
    "Consider removing or re-installing these packages.",
    warningf
  )

  pkgs[!missing]

}

renv_snapshot_description <- function(path) {

  dcf <- catch(renv_description_read(path))
  if (inherits(dcf, "error"))
    return(dcf)

  dcf[["Source"]] <- renv_snapshot_description_source(dcf)
  dcf[["Hash"]] <- renv_hash_description(path)

  fields <- c("Package", "Version", "Source")
  missing <- setdiff(fields, names(dcf))
  if (length(missing)) {
    fmt <- "required fields %s missing from DESCRIPTION at path '%s'"
    msg <- sprintf(fmt, paste(shQuote(missing), collapse = ", "), path)
    return(simpleError(msg))
  }

  fields <- c(fields, grep("^Remote", names(dcf), value = TRUE), "Hash")
  as.list(dcf[fields])

}

renv_snapshot_description_source <- function(dcf) {

  if (!is.null(dcf[["biocViews"]]))
    return("Bioconductor")

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
    renv_pretty_print(
      renv_records_select(new, actions, "install"),
      "The following package(s) will be added to the lockfile:"
    )
  }

  if ("remove" %in% actions) {
    renv_pretty_print(
      renv_records_select(old, actions, "remove"),
      "The following package(s) will be removed from the lockfile:"
    )
  }

  if ("upgrade" %in% actions) {
    renv_pretty_print_pair(
      renv_records_select(old, actions, "upgrade"),
      renv_records_select(new, actions, "upgrade"),
      "The following package(s) will be upgraded in the lockfile:"
    )
  }

  if ("downgrade" %in% actions) {
    renv_pretty_print_pair(
      renv_records_select(old, actions, "downgrade"),
      renv_records_select(new, actions, "downgrade"),
      "The following package(s) will be downgraded in the lockfile:"
    )
  }

  if ("crossgrade" %in% actions) {
    renv_pretty_print_pair(
      renv_records_select(old, actions, "crossgrade"),
      renv_records_select(new, actions, "crossgrade"),
      "The following package(s) will be modified in the lockfile:"
    )
  }

  # report changes to other fields
  squish <- function(item) {

    squished <- case(
      empty(item)     ~ "",
      !is_named(item) ~ paste(item, collapse = ", "),
      paste(names(item), item, sep = "=", collapse = ", ")
    )

    if (!nzchar(squished))
      return("<empty>")

    sprintf("[%s]", trunc(squished, 24))

  }

  old$R$Packages <- new$R$Packages <- list()
  diff <- renv_lockfile_diff(old, new, function(lhs, rhs) {
    paste(squish(lhs), squish(rhs), sep = " => ")
  })

  writeLines("The following lockfile fields will be updated:\n")
  output <- stack()
  renv_lockfile_write(diff, delim = ": ", emitter = output$push)
  writeLines(paste("  ", output$data(), sep = ""))

}

