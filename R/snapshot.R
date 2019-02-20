
#' Snapshot a Virtual Environment
#'
#' Snapshot a virtual environment, creating a **lockfile** file that can be used
#' to later restore the virtual environment. See the [lockfile] documentation
#' for more details on the structure of a lockfile.
#'
#' @inheritParams renv-params
#'
#' @param file The location to where the generated lockfile should be written.
#'   When `NULL`, the lockfile (as an \R object) is returned directly instead.
#'
#' @family reproducibility
#'
#' @export
snapshot <- function(project = NULL,
                     file = file.path(project, "renv.lock"),
                     confirm = interactive())
{
  project <- project %||% renv_state$project()

  new <- renv_lockfile_init()
  new$R$Package <- renv_snapshot_r_packages()

  if (is.null(file))
    return(new)

  old <- list()
  if (file.exists(file)) {
    old <- renv_lockfile_read(file)
    diff <- renv_lockfile_diff(old, new)
    if (empty(diff)) {
      vmessagef("* The lockfile is already up-to-date.")
      return(invisible(new))
    }
  }

  # check for missing dependencies and warn if any are discovered
  if (!renv_snapshot_validate_dependencies(new, confirm))
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

renv_snapshot_validate_dependencies <- function(lockfile, confirm) {

  packages <- lockfile$R$Package

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
      paste(sprintf("\t%s: %s", names(bad), map_chr(bad, toString)), collapse = "\n"),
      "",
      "Consider re-installing these packages before snapshotting the lockfile."
    )
    message(text)

    if (!proceed()) {
      message("Operation aborted.")
      return(FALSE)
    }

  }

  TRUE

}

renv_snapshot_r_packages <- function(synchronize = FALSE) {

  # list packages in the library
  library <- renv_libpaths_default()
  paths <- list.files(library, full.names = TRUE)

  # remove 'base' packages
  ip <- renv_installed_packages_base()
  paths <- paths[!basename(paths) %in% c(ip$Package, "translations")]

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

  # copy packages into the cache during snapshot if requested
  if (synchronize)
    lapply(records, renv_cache_synchronize)

  # name results and return
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
  missing <- !renv_file_exists(desc)
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

renv_snapshot_description <- function(path) {

  info <- file.info(path)
  if (identical(info$isdir, TRUE))
    path <- file.path(path, "DESCRIPTION")

  if (!renv_file_exists(path)) {
    fmt <- "No DESCRIPTION at path '%s'."
    msg <- sprintf(fmt, path)
    return(simpleError(msg))
  }

  # TODO: Check for tempfiles that sneak into library path, e.g. 'file<abcd>'
  # Report and skip?
  dcf <- catch(renv_dcf_read(path))
  if (inherits(dcf, "error"))
    return(dcf)

  dcf[["Source"]] <- renv_snapshot_description_source(dcf)
  dcf[["Hash"]] <- renv_hash_description(path)

  fields <- c("Package", "Version", "Source", "Hash")
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
    msg <- "The following package(s) will be added to the lockfile:"
    renv_pretty_print(msg, new, actions, "install")
  }

  if ("remove" %in% actions) {
    msg <- "The following package(s) will be removed from the lockfile:"
    renv_pretty_print(msg, old, actions, "remove")
  }

  if ("upgrade" %in% actions) {
    msg <- "The following package(s) will be upgraded in the lockfile:"
    renv_pretty_print_pair(msg, old, new, actions, "upgrade")
  }

  if ("downgrade" %in% actions) {
    msg <- "The following package(s) will be downgraded in the lockfile:"
    renv_pretty_print_pair(msg, old, new, actions, "downgrade")
  }

  if ("crossgrade" %in% actions) {
    msg <- "The following package(s) will be modified in the lockfile:"
    renv_pretty_print_pair(msg, old, new, actions, "crossgrade")
  }

}

