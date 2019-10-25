
#' Snapshot a Project
#'
#' Call `snapshot()` to create a **lockfile** capturing the state of a project's
#' \R package dependencies. The lockfile can be used to later restore these
#' project's dependencies as required. See the [lockfile] documentation for more
#' details on the structure of a lockfile.
#'
#' @section Snapshot Type:
#'
#' Depending on how you prefer to manage dependencies, you might prefer
#' selecting a different snapshot mode. The modes available are as follows:
#'
#' \describe{
#'
#' \item{`"simple"`}{
#' Capture all packages within the active \R libraries in the lockfile.
#' This is the quickest and simplest method, but may lead to undesired
#' packages (e.g. development dependencies) entering the lockfile.
#' }
#'
#' \item{`"packrat"`}{
#' Perform a Packrat-style snapshot. The intersection of packages discovered in
#' your \R libraries, alongside those discovered in your \R code by
#' `renv::dependencies()`, will enter the lockfile. This helps ensure that only
#' the packages you are using will enter the lockfile, but may be slower if your
#' project contains a large number of files. If this becomes an issue, you might
#' consider using `.renvignore` files to limit which files `renv` uses for
#' dependency discovery, or explicitly declaring your required dependencies in a
#' `DESCRIPTION` file.
#' }
#'
#' \item{`"custom"`}{
#' Like `"packrat"`, but use a custom user-defined filter instead. The filter
#' should be specified by the \R option `renv.snapshot.filter`, and should
#' either be a character vector naming a function (e.g. `"package::method"`),
#' or be a function itself. The function should only accept one argument (the
#' project directory), and should return a vector of package names to include
#' in the lockfile.
#' }
#'
#' }
#'
#' By default, `"packrat"`-style snapshots are used. The snapshot type can be
#' configured on a project-specific basis using the `renv` project [settings]
#' mechanism.
#'
#' @inherit renv-params
#'
#' @param library The \R libraries to snapshot. When `NULL`, the active \R
#'   libraries (as reported by `.libPaths()`) are used.
#'
#' @param lockfile The location where the generated lockfile should be written.
#'   By default, the lockfile is written to a file called `renv.lock` in the
#'   project directory. When `NULL`, the lockfile (as an \R object) is returned
#'   directly instead.
#'
#' @param type The type of snapshot to perform. See **Snapshot Type** for
#'   more details. When `NULL` (the default), a "packrat"-style snapshot
#'   is performed.
#'
#' @param force Boolean; force generation of a lockfile even when preflight
#'   validation checks have failed?
#'
#' @return The generated lockfile, as an \R object (invisibly). Note that
#'   this function is normally called for its side effects.
#'
#' @family reproducibility
#'
#' @export
#'
#' @example examples/examples-init.R
snapshot <- function(project  = NULL,
                     ...,
                     library  = NULL,
                     lockfile = file.path(project, "renv.lock"),
                     type     = settings$snapshot.type(project = project),
                     confirm  = interactive(),
                     force    = FALSE)
{
  renv_consent_check()
  renv_scope_error_handler()

  project <- project %||% renv_project()
  library <- library %||% renv_libpaths_all()

  if (renv_config("snapshot.preflight", default = TRUE))
    renv_snapshot_preflight(project, library)

  new <- renv_lockfile_init(project)
  renv_records(new) <-
    renv_snapshot_r_packages(library = library) %>%
    renv_snapshot_filter(project = project, type = type) %>%
    renv_snapshot_fixup()

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
  validated <- renv_snapshot_validate(project, new, library)
  if (!validated && !force) {
    if (confirm && !proceed()) {
      message("* Operation aborted.")
      return(invisible(new))
    } else if (!interactive()) {
      stop("aborting snapshot due to pre-flight validation failure")
    }
  }

  # report actions to the user
  actions <- renv_lockfile_diff_packages(old, new)
  if (confirm || renv_verbose())
    renv_snapshot_report_actions(actions, old, new)

  # request user confirmation
  if (confirm && !proceed()) {
    message("* Operation aborted.")
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

renv_snapshot_validate <- function(project, lockfile, library) {

  # allow user to disable snapshot validation, just in case
  enabled <- renv_config("snapshot.validate", default = TRUE)
  if (!enabled)
    return(TRUE)

  all(
    renv_snapshot_validate_bioconductor(project, lockfile, library),
    renv_snapshot_validate_dependencies_available(project, lockfile, library),
    renv_snapshot_validate_dependencies_compatible(project, lockfile, library),
    renv_snapshot_validate_sources(project, lockfile, library)
  )
}

renv_snapshot_validate_bioconductor <- function(project, lockfile, library) {

  # check whether any packages are installed from Bioconductor
  records <- renv_records(lockfile)
  sources <- extract_chr(records, "Source")
  if (!"Bioconductor" %in% sources)
    return(TRUE)

  package <- if (getRversion() >= "3.5.0") "BiocManager" else "BiocInstaller"
  if (package %in% names(records))
    return(TRUE)

  text <- c(
    "One or more Bioconductor packages are used in your project,",
    "but neither BiocManager nor BiocInstaller will be snapshotted.",
    "",
    "Consider installing the appropriate package before snapshot.",
    ""
  )

  writeLines(text)
  FALSE

}

renv_snapshot_validate_dependencies_available <- function(project, lockfile, library) {

  # use library to collect package dependency versions
  records <- renv_records(lockfile)
  packages <- extract_chr(records, "Package")
  locs <- find.package(packages, lib.loc = library, quiet = TRUE)
  deps <- bapply(locs, renv_dependencies_discover_description)
  if (empty(deps))
    return(TRUE)

  splat <- split(deps, deps$Package)

  # exclude base R packages
  splat <- splat[renv_vector_diff(names(splat), renv_packages_base())]

  # check for required packages not currently installed
  requested <- names(splat)
  missing <- renv_vector_diff(requested, packages)
  if (empty(missing))
    return(TRUE)

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

  FALSE

}

renv_snapshot_validate_dependencies_compatible <- function(project, lockfile, library) {

  # use library to collect package dependency versions
  records <- renv_records(lockfile)
  packages <- extract_chr(records, "Package")
  locs <- find.package(packages, lib.loc = library, quiet = TRUE)
  deps <- bapply(locs, renv_dependencies_discover_description)
  if (empty(deps))
    return(TRUE)

  splat <- split(deps, deps$Package)

  # exclude base R packages
  splat <- splat[renv_vector_diff(names(splat), renv_packages_base())]

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

  fmt <- "%s requires %s, but %s will be snapshotted"
  txt <- sprintf(fmt, format(package), format(requires), format(request))

  renv_pretty_print(
    txt,
    "The following package(s) have unsatisfied dependencies:",
    "Consider updating the required dependencies as appropriate.",
    wrap = FALSE
  )

  FALSE

}

renv_snapshot_validate_sources <- function(project, lockfile, library) {

  records <- renv_records(lockfile)

  if (renv_testing())
    records$renv <- NULL

  unknown <- Filter(
    function(record) (record$Source %||% "unknown") == "unknown",
    records
  )

  if (empty(unknown))
    return(TRUE)

  # nocov start
  renv_pretty_print(
    names(unknown),
    "The following package(s) were installed from an unknown source:",
    c(
      "renv may be unable to restore these packages in the future.",
      "Consider re-installing these packages from a known source (e.g. CRAN)."
    )
  )

  FALSE

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
  paths <- paths[!basename(paths) %in% settings$ignored.packages()]

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
  names(records) <- basename(packages)

  # report any snapshot failures
  broken <- Filter(function(record) inherits(record, "error"), records)
  if (length(broken)) {

    messages <- map_chr(broken, conditionMessage)
    text <- sprintf("'%s': %s", names(broken), messages)

    renv_pretty_print(
      text,
      "renv was unable to snapshot the following packages:",
      "These packages will likely need to be repaired and / or reinstalled."
    )

    stopf("snapshot of library %s failed", shQuote(aliased_path(library)))

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
    "The following package(s) are missing their DESCRIPTION files:",
    c(
      "Consider removing or re-installing these packages.",
      paste("Library:", shQuote(aliased_path(library), type = "cmd"))
    ),
    wrap = FALSE
  )

  pkgs[!missing]

}

renv_snapshot_description <- function(path = NULL, package = NULL) {

  path <- path %||% renv_package_find(package)
  dcf <- catch(renv_description_read(path, package))
  if (inherits(dcf, "error"))
    return(dcf)

  source <- renv_snapshot_description_source(dcf)
  dcf[names(source)] <- source
  dcf[["Hash"]] <- renv_hash_description(path)

  fields <- c("Package", "Version", "Source")
  missing <- renv_vector_diff(fields, names(dcf))
  if (length(missing)) {
    fmt <- "required fields %s missing from DESCRIPTION at path '%s'"
    msg <- sprintf(fmt, paste(shQuote(missing), collapse = ", "), path)
    return(simpleError(msg))
  }

  remotes <- grep("^Remote", names(dcf), value = TRUE)
  all <- c(fields, "Repository", remotes, "Hash")
  keep <- renv_vector_intersect(all, names(dcf))
  as.list(dcf[keep])

}

renv_snapshot_description_source <- function(dcf) {

  if (!is.null(dcf[["biocViews"]]))
    return(list(Source = "Bioconductor"))

  if (!is.null(dcf[["Repository"]]))
    return(list(Source = "Repository", Repository = dcf[["Repository"]]))

  type <- dcf[["RemoteType"]]
  if (!is.null(type))
    return(list(Source = renv_alias(type)))

  package <- dcf$Package
  if (is.null(package))
    return(list(Source = "unknown"))

  entry <- local({
    renv_scope_options(renv.verbose = FALSE)
    catch(renv_available_packages_entry(package = package, type = "source"))
  })

  if (!inherits(entry, "error"))
    return(list(Source = "Repository", Repository = entry[["Name"]]))

  list(Source = "unknown")

}

# nocov start
renv_snapshot_report_actions <- function(actions, old, new) {

  if (!renv_verbose() || empty(actions))
    return(invisible())

  lhs <- renv_records(old)
  rhs <- renv_records(new)
  renv_pretty_print_records_pair(
    lhs[names(lhs) %in% names(actions)],
    rhs[names(rhs) %in% names(actions)],
    "The following package(s) will be updated in the lockfile:"
  )

}
# nocov end

renv_snapshot_auto <- function(project) {

  # don't auto-snapshot if disabled by user
  if (!renv_config("auto.snapshot", default = FALSE))
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

  # don't auto-snapshot unless the active library is the project library
  if (!renv_file_same(renv_libpaths_default(), library))
    return(FALSE)

  # passed pre-flight checks; snapshot the library
  # validation messages can be noisy; turn off for auto snapshot
  status <- local({
    renv_scope_options(renv.config.snapshot.validate = FALSE, renv.verbose = FALSE)
    catch(snapshot(project = project, confirm = FALSE))
  })

  if (inherits(status, "error"))
    return(FALSE)

  lockfile <- file.path(project, "renv.lock")
  vwritef("* Lockfile written to '%s'.", aliased_path(lockfile))
  TRUE

}

renv_snapshot_filter <- function(project, records, type) {

  start <- Sys.time()

  type <- type %||% settings$snapshot.type(project = project)
  result <- switch(type,
    simple  = renv_snapshot_filter_simple(project, records),
    packrat = renv_snapshot_filter_packrat(project, records),
    custom  = renv_snapshot_filter_custom(project, records),
    stopf("unknown snapshot type '%s'", type)
  )

  end <- Sys.time()

  # report if dependency discovery took a long time
  limit <- renv_config("snapshot.filter.timelimit", default = 10L)
  if (difftime(end, start, units = "secs") > limit) {

    lines <- c(
      "NOTE: Dependency discovery took %s %s during snapshot.",
      "Consider using .renvignore to ignore files -- see `?dependencies` for more information.",
      "Use `renv::settings$snapshot.type(\"simple\")` to disable dependency discovery during snapshot."
    )

    time <- difftime(end, start, units = "auto")
    elapsed <- format(unclass(signif(time, digits = 2L)))
    units <- switch(
      attr(time, "units"),
      secs  = "seconds",
      mins  = "minutes",
      hours = "hours",
      days  = "days",
      weeks = "weeks"
    )

    vwritef(lines, elapsed, units)

  }

  result

}

renv_snapshot_filter_simple <- function(project, records) {
  records
}

renv_snapshot_filter_packrat <- function(project, records) {

  # keep only package records for packages actually used in project
  deps <- dependencies(project, quiet = TRUE)
  ignored <- settings$ignored.packages(project = project)
  packages <- renv_vector_diff(unique(deps$Package), ignored)
  paths <- renv_package_dependencies(packages, project = project)
  all <- as.character(names(paths))
  kept <- keep(records, all)

  # add in bioconductor infrastructure packages
  # if any other bioconductor packages detected
  sources <- extract_chr(kept, "Source")
  if ("Bioconductor" %in% sources) {
    packages <- c("BiocManager", "BiocInstaller", "BiocVersion")
    for (package in packages)
      kept[[package]] <- records[[package]]
  }

  kept

}

renv_snapshot_filter_custom <- function(project, records) {

  option <- "renv.snapshot.filter"
  filter <- getOption(option, default = NULL)

  # allow for filter naming a function to use
  if (is.character(filter))
    filter <- eval(parse(text = filter), envir = baseenv())

  # check we got a function
  if (!is.function(filter)) {
    fmt <- "snapshot of type '%s' requested, but '%s' is not a function"
    stopf(fmt, "custom", option)
  }

  # invoke the filter to get package list
  packages <- filter(project)

  if (empty(packages))
    return(records)

  if (!is.character(packages))
    stop("custom snapshot filter did not return a character vector")

  keep(records, packages)

}

renv_snapshot_fixup <- function(records) {

  records <- renv_snapshot_fixup_renv(records)
  records

}

renv_snapshot_fixup_renv <- function(records) {

  if (renv_testing())
    return(records)

  record <- records$renv
  if (is.null(record) || !identical(record$Source, "unknown"))
    return(records)

  remote <- paste("rstudio/renv", record$Version, sep = "@")
  records$renv <- renv_remotes_resolve(remote)
  records

}
