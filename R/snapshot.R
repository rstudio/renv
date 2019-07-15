
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
#' @param library The \R library to snapshot. When `NULL`, the active library
#'   (as reported by `.libPaths()`) is used.
#'
#' @param lockfile The location where the generated lockfile should be written.
#'   By default, the lockfile is written to a file called `renv.lock` in the
#'   project directory. When `NULL`, the lockfile (as an \R object) is returned
#'   directly instead.
#'
#' @family reproducibility
#'
#' @export
snapshot <- function(project  = NULL,
                     ...,
                     library  = NULL,
                     lockfile = file.path(project, "renv.lock"),
                     confirm  = interactive())
{
  renv_scope_error_handler()

  project <- project %||% renv_project()
  library <- library %||% renv_libpaths_default()

  renv_snapshot_preflight(project, library)

  new <- renv_lockfile_init(project)
  new$R$Package <- renv_snapshot_r_packages(library = library)

  if (is.null(lockfile))
    return(new)

  # TODO: do we still want to snapshot if the user cancels
  # the R-level snapshot?
  on.exit(renv_python_snapshot(project), add = TRUE)

  old <- list()
  if (file.exists(lockfile)) {
    old <- renv_lockfile_read(lockfile)
    diff <- renv_lockfile_diff(old, new)
    if (empty(diff)) {
      vwritef("* The lockfile is already up to date.")
      return(invisible(new))
    }
  }

  # check for missing dependencies and warn if any are discovered
  # TODO: enable this check for multi-library configurations
  validated <-
    length(library) > 1 ||
    renv_snapshot_validate(project, new, library, confirm)

  if (!validated) {
    message("* Operation aborted.")
    return(invisible(new))
  }

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
  renv_infrastructure_write_rbuildignore(project)

  invisible(new)

}

renv_snapshot_preflight <- function(project, library) {
  lapply(library, renv_snapshot_preflight_impl, project = project)
}

renv_snapshot_preflight_impl <- function(project, library) {
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

renv_snapshot_validate <- function(project, lockfile, library, confirm) {

  # allow user to disable snapshot validation, just in case
  enabled <- renv_config("snapshot.validate", default = TRUE)
  if (!enabled)
    return(TRUE)

  renv_snapshot_validate_dependencies(project, lockfile, library, confirm) &&
    renv_snapshot_validate_sources(project, lockfile, library, confirm)
}

renv_snapshot_validate_dependencies <- function(project, lockfile, library, confirm) {

  # use library to collect package dependency versions
  records <- renv_records(lockfile)
  packages <- extract_chr(records, "Package")
  locs <- file.path(library, packages)
  deps <- bapply(locs, renv_dependencies_discover_description)
  if (empty(deps))
    return(TRUE)

  splat <- split(deps, deps$Package)

  # exclude base R packages
  splat <- splat[setdiff(names(splat), renv_packages_base())]

  # check for required packages not currently installed
  requested <- names(splat)
  missing <- setdiff(requested, packages)
  if (length(missing)) {

    usedby <- map_chr(missing, function(package) {

      revdeps <- sort(unique(basename(deps$Source)[deps$Package == package]))

      items <- revdeps; limit <- 3
      if (length(revdeps) > limit) {
        rest <- length(revdeps) - limit
        suffix <- paste("and", length(revdeps) - 3, plural("other", rest))
        items <- c(revdeps[seq_len(limit)], suffix)
      }

      paste(items, collapse = ", ")

    })

    renv_pretty_print(
      sprintf("%s  [required by %s]", format(missing), usedby),
      "The following required packages are not installed:",
      "Consider re-installing these packages before snapshotting the lockfile.",
      wrap = FALSE
    )

    if (confirm && !proceed())
      return(FALSE)

  }

  # collapse requirements for each package
  bad <- enumerate(splat, function(package, requirements) {

    # skip NULL records (should be handled above)
    record <- records[[package]]
    if (is.null(record))
      return(NULL)

    version <- record$Version

    # drop packages without explicit version requirement
    requirements <- requirements[nzchar(requirements$Require), ]
    if (nrow(requirements) == 0)
      return(NULL)

    # add in requested version
    requirements$Requested <- version

    # generate expressions to evaluate
    fmt <- "package_version('%s') %s package_version('%s')"
    code <- with(requirements, sprintf(fmt, Requested, Require, Version))
    parsed <- parse(text = code)
    ok <- map_lgl(parsed, eval, envir = baseenv())

    # return requirements that weren't satisfied
    requirements[!ok, ]

  })

  bad <- bind_list(bad)
  if (empty(bad))
    return(TRUE)

  package  <- basename(bad$Source)
  requires <- sprintf("%s (%s %s)", bad$Package, bad$Require, bad$Version)
  request  <- bad$Requested

  fmt <- "- %s requires %s, but %s will be snapshotted"
  warningf(fmt, format(package), format(requires), format(request))

  return(FALSE)
}

renv_snapshot_validate_sources <- function(project, lockfile, library, confirm) {

  records <- renv_records(lockfile)
  unknown <- Filter(
    function(record) (record$Source %||% "unknown") == "unknown",
    records
  )

  if (empty(unknown))
    return(TRUE)

  # nocov start
  if (confirm || renv_verbose()) {

    renv_pretty_print(
      names(unknown),
      "The following package(s) were installed from an unknown source:",
      c(
        "renv may be unable to restore these packages in the future.",
        "Consider re-installing these packages from a known source (e.g. CRAN)."
      )
    )

    if (confirm && !proceed())
      return(FALSE)

  }
  # nocov end

  TRUE

}

# NOTE: if packages are found in multiple libraries,
# then the first package found in the library paths is
# kept and others are discarded
renv_snapshot_r_packages <- function(library = NULL) {
  records <- uapply(library, renv_snapshot_r_packages_impl)
  dupes <- duplicated(names(records))
  records[!dupes]
}

renv_snapshot_r_packages_impl <- function(library = NULL) {

  # list packages in the library
  library <- library %||% renv_libpaths_default()
  paths <- list.files(library, full.names = TRUE)

  # remove 'base' packages
  ip <- renv_installed_packages_base()
  paths <- paths[!basename(paths) %in% c(ip$Package, "translations")]

  # remove ignored packages
  paths <- paths[!basename(paths) %in% c("renv", settings$ignored.packages())]

  # ignore '_cache' folder explicitly (written by 'pak')
  paths <- paths[!basename(paths) %in% "_cache"]

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
    wrap = FALSE
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
  if (remote %in% c("git2r", "xgit"))
    return("Git")

  renv_alias(remote)

}

# nocov start
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

  if (empty(old))
    return(invisible())

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

  # only report packages which are being modified; not added / removed
  keep <- names(actions)[actions %in% c("upgrade", "downgrade", "crossgrade")]
  old$R$Package <- old$R$Package[keep]
  new$R$Package <- new$R$Package[keep]

  # perform the diff
  diff <- renv_lockfile_diff(old, new, function(lhs, rhs) {
    paste(squish(lhs), squish(rhs), sep = " => ")
  })

  if (empty(diff))
    return(invisible())

  # report it
  writeLines("The following lockfile fields will be updated:\n")
  output <- stack()
  renv_lockfile_write(diff, delim = ": ", emitter = output$push)
  writeLines(paste("  ", output$data(), sep = ""))

}
# nocov end

renv_snapshot_auto <- function(project) {

  # don't auto-snapshot if disabled by user
  if (!renv_config("auto.snapshot", default = TRUE))
    return(FALSE)

  # only automatically snapshot the current project
  if (!identical(project, renv_project(default = NULL)))
    return(FALSE)

  # don't auto-snapshot if the project hasn't been initialized
  if (!renv_project_initialized(project = project))
    return(FALSE)

  # don't auto-snapshot if we don't have a library
  library <- renv_paths_library(project = project)
  if (!file.exists(library))
    return(FALSE)

  # passed pre-flight checks; snapshot the library
  # validation messages can be noisy; turn off for auto snapshot
  renv_scope_options(renv.config.snapshot.validate = FALSE)
  snapshot(project = project, library = library, confirm = FALSE)

}

# nocov start
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
# nocov end
