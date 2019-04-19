
#' Snapshot a Project
#'
#' Call `snapshot()` to create a **lockfile** capturing the state of a project's
#' \R package dependencies. The lockfile can be used to later restore these
#' project's dependencies as required. See the [lockfile] documentation for more
#' details on the structure of a lockfile.
#'
#' When no project library is active, `snapshot()` will capture only the
#' packages within use (as detected by `dependencies()`) within a project.
#'
#' @inheritParams renv-params
#'
#' @param library The \R library to snapshot. When `NULL`, the project library
#'   associated with the requested project is used.
#'
#' @param lockfile The location where the generated lockfile should be written.
#'   When `NULL`, the lockfile (as an \R object) is returned directly instead.
#'
#' @family reproducibility
#'
#' @export
snapshot <- function(project  = NULL,
                     library  = NULL,
                     lockfile = file.path(project, "renv.lock"),
                     confirm  = interactive())
{
  renv_scope_error_handler()

  project <- project %||% renv_project()

  # if the user calls snapshot without an active renv project,
  # then take this as a request to snapshot the active library
  if (renv_project_initialized(project)) {
    library <- library %||% renv_paths_library(project = project)
    filter  <- NULL
  } else {
    library <- library %||% renv_libpaths_default()
    filter  <- renv_snapshot_filter(project = project)
  }

  renv_snapshot_preflight(project, library)

  new <- renv_lockfile_init(project)

  records <- renv_snapshot_r_packages(library = library)
  new$R$Package <- renv_snapshot_filter_apply(records, filter)

  # TODO: do we still want to snapshot if the user cancels
  # the R-level snapshot?
  on.exit(renv_python_snapshot(project), add = TRUE)

  old <- NULL
  if (file.exists(lockfile))
    old <- renv_lockfile_read(lockfile)

  new$Python <- old$Python
  if (is.null(lockfile))
    return(new)

  diff <- renv_lockfile_diff(old, new)
  if (empty(diff)) {
    vwritef("* The lockfile is already up to date.")
    return(invisible(new))
  }

  # check for missing dependencies and warn if any are discovered
  if (!renv_snapshot_validate(project, new, confirm))
    return(invisible(new))

  # report actions to the user
  actions <- renv_lockfile_diff_packages(old, new)
  if (confirm && renv_verbose()) {
    renv_snapshot_report_actions(actions, old, new)
    vwritef("The lockfile will be written to '%s'.", aliased_path(lockfile))
  }

  # request user confirmation
  if (confirm && !proceed()) {
    message("Operation aborted.")
    return(invisible(new))
  }

  # write it out
  ensure_parent_directory(lockfile)
  renv_lockfile_write(new, file = lockfile)
  vwritef("* Lockfile written to '%s'.", aliased_path(lockfile))

  # ensure the lockfile is .Rbuildignore-d
  renv_write_rbuildignore(project)

  invisible(new)

}

renv_snapshot_preflight <- function(project, library) {
  renv_snapshot_preflight_library_exists(project, library)
}

renv_snapshot_preflight_library_exists <- function(project, library) {

  # check that we have a directory
  type <- renv_file_type(library, symlinks = FALSE)
  if (type == "directory")
    return(TRUE)

  # if the file exists but isn't a directory, fail
  if (nzchar(type)) {
    fmt <- "library '%s' exists but is not a directory"
    stopf(fmt, aliased_path(library))
  }

  # the directory doesn't exist; perhaps the user hasn't called init
  if (identical(library, renv_paths_library(project = project))) {
    fmt <- "project '%s' has no private library -- have you called `renv::init()`?"
    stopf(fmt, aliased_path(project))
  }

  # user tried to snapshot arbitrary but missing path
  fmt <- "library '%s' does not exist; cannot proceed"
  stopf(fmt, aliased_path(library))

}

renv_snapshot_validate <- function(project, lockfile, confirm) {
  all(
    renv_snapshot_validate_dependencies(project, lockfile, confirm),
    renv_snapshot_validate_sources(project, lockfile, confirm)
  )
}

renv_snapshot_validate_dependencies <- function(project, lockfile, confirm) {

  records <- renv_records(lockfile)
  installed <- renv_installed_packages()
  missing <- lapply(records, function(record) {
    path <- renv_paths_library(project = project, record$Package)
    deps <- renv_dependencies_discover_description(path)
    setdiff(deps$Package, c("R", installed$Package))
  })

  bad <- Filter(length, missing)
  if (!length(bad))
    return(TRUE)

  keys <- names(bad)
  vals <- lapply(bad, function(packages) {

    n <- length(packages)
    if (n > 3) {
      fmt <- "and %i %s"
      other <- sprintf(fmt, n - 3, plural("other", n - 3))
      packages <- c(packages[1:3], other)
    }

    paste(packages, collapse = ", ")

  })

  renv_pretty_print(
    sprintf("%s: [%s]", keys, vals),
    "The following package(s) depend on packages which are not currently installed:",
    "Consider re-installing these packages before snapshotting the lockfile.",
    wrap = FALSE
  )

  if (confirm && !proceed()) {
    message("Operation aborted.")
    return(FALSE)
  }

  TRUE

}

renv_snapshot_validate_sources <- function(project, lockfile, confirm) {

  records <- renv_records(lockfile)
  unknown <- Filter(
    function(record) (record$Source %||% "") == "unknown",
    records
  )

  if (empty(unknown))
    return(TRUE)

  if (confirm) {

    renv_pretty_print(
      names(unknown),
      "The following package(s) were installed from an unknown source:",
      "Consider re-installing these packages from a known source (e.g. CRAN)."
    )

    if (!proceed()) {
      message("Operation aborted.")
      return(FALSE)
    }

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

  renv_pretty_print(
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

  renv_pretty_print(
    basename(pkgs)[missing],
    "The following folder(s) appear to be left-over temporary directories:",
    "Consider removing these folders from your library."
  )

  pkgs[!missing]

}

renv_snapshot_r_library_diagnose_missing_description <- function(library, pkgs) {

  desc <- file.path(pkgs, "DESCRIPTION")
  missing <- !file.exists(desc)
  if (!any(missing))
    return(pkgs)

  renv_pretty_print(
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
    renv_pretty_print_records(
      renv_records_select(new, actions, "install"),
      "The following package(s) will be added to the lockfile:"
    )
  }

  if ("remove" %in% actions) {
    renv_pretty_print_records(
      renv_records_select(old, actions, "remove"),
      "The following package(s) will be removed from the lockfile:"
    )
  }

  if ("upgrade" %in% actions) {
    renv_pretty_print_records_pair(
      renv_records_select(old, actions, "upgrade"),
      renv_records_select(new, actions, "upgrade"),
      "The following package(s) will be upgraded in the lockfile:"
    )
  }

  if ("downgrade" %in% actions) {
    renv_pretty_print_records_pair(
      renv_records_select(old, actions, "downgrade"),
      renv_records_select(new, actions, "downgrade"),
      "The following package(s) will be downgraded in the lockfile:"
    )
  }

  if ("crossgrade" %in% actions) {
    renv_pretty_print_records_pair(
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

    sprintf("[%s]", trunc(squished, 32))

  }

  if (empty(old))
    return()

  # only report packages which are being modified; not added / removed
  keep <- names(actions)[actions %in% c("upgrade", "downgrade", "crossgrade")]
  old$R$Package <- old$R$Package[keep]
  new$R$Package <- new$R$Package[keep]

  # perform the diff
  diff <- renv_lockfile_diff(old, new, function(lhs, rhs) {
    paste(squish(lhs), squish(rhs), sep = " => ")
  })

  if (empty(diff))
    return()

  writeLines("The following lockfile fields will be updated:\n")
  output <- stack()
  renv_lockfile_write(diff, delim = ": ", emitter = output$push)
  writeLines(paste("  ", output$data(), sep = ""))

}

renv_snapshot_auto <- function(project) {

  # only automatically snapshot the current project
  if (!identical(project, renv_project(default = NULL)))
    return(FALSE)

  # don't auto-snapshot if the project hasn't been initialized
  if (!renv_project_initialized(project = project))
    return(FALSE)

  # don't auto-snapshot if the user has explicitly disabled it
  if (!settings$auto.snapshot(project = project))
    return(FALSE)

  # don't auto-snapshot if we don't have a library
  library <- renv_paths_library(project = project)
  if (!file.exists(library))
    return(FALSE)

  # passed pre-flight checks; snapshot the library
  snapshot(project = project, library = library, confirm = FALSE)

}

renv_snapshot_filter <- function(project) {
  deps <- dependencies(project)
  ignored <- c("renv", settings$ignored.packages(project = project))
  packages <- setdiff(unique(deps$Package), ignored)
  paths <- renv_dependencies(project, packages)
  as.character(names(paths))
}

renv_snapshot_filter_apply <- function(records, filter) {

  if (is.null(filter))
    return(records)

  if (is.function(filter))
    return(Filter(filter, records))

  if (is.character(filter))
    return(records[intersect(filter, names(records))])

  stopf("invalid filter in call to snapshot")

}
