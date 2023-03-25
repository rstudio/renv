
`_renv_status_running` <- FALSE

#' Status
#'
#' Report differences between the project's lockfile and the current state of
#' the project's library (if any).
#'
#' @inherit renv-params
#'
#' @param library The library paths. By default, the library paths associated
#'   with the requested project are used.
#'
#' @param lockfile The path to a lockfile. By default, the project lockfile
#'   (called `renv.lock`) is used.
#'
#' @param sources Boolean; check that each of the recorded packages have a
#'   known installation source? If a package has an unknown source, `renv`
#'   may be unable to restore it.
#'
#' @param cache Boolean; perform diagnostics on the global package cache?
#'   When `TRUE`, `renv` will validate that the packages installed into the
#'   cache are installed at the expected + proper locations, and validate the
#'   hashes used for those storage locations.
#'
#' @return This function is normally called for its side effects.
#'
#' @export
#'
#' @example examples/examples-init.R
status <- function(project = NULL,
                   ...,
                   library = NULL,
                   lockfile = NULL,
                   sources = TRUE,
                   cache = FALSE)
{
  renv_scope_error_handler()
  renv_dots_check(...)

  renv_snapshot_auto_suppress_next()
  renv_scope_options(renv.prompt.enabled = FALSE)

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  libpaths <- renv_libpaths_resolve(library)
  lockpath <- lockfile %||% renv_lockfile_path(project)

  invisible(renv_status_impl(project, libpaths, lockpath, sources, cache))
}

renv_status_impl <- function(project, libpaths, lockpath, sources, cache) {

  default <- list(library = list(), lockfile = list(), synchronized = FALSE)

  # check to see if we've initialized this project
  if (!renv_project_initialized(project)) {
    vwritef("* This project has not yet been initialized.")
    return(default)
  }

  # mark status as running
  `_renv_status_running` <<- TRUE
  on.exit(`_renv_status_running` <<- FALSE, add = TRUE)

  # check for existing lockfile, library
  ok <-
    renv_status_check_missing_library(project, libpaths) &&
    renv_status_check_missing_lockfile(project, lockpath)

  if (!ok)
    return(default)

  # get lockfile state
  lockfile <- renv_lockfile_read(lockpath)

  # get library state
  library <- local({
    renv_scope_options(renv.verbose = FALSE)
    snapshot(project  = project,
             library  = libpaths,
             lockfile = NULL,
             type     = "all",
             force    = TRUE)
  })

  # get dependencies
  dependencies <- renv_snapshot_dependencies(
    project = project,
    type    = settings$snapshot.type(project = project)
  )

  # include transitive dependencies
  packages <- sort(unique(c(dependencies$Package, "renv")))
  paths <- renv_package_dependencies(packages, project = project)
  packages <- as.character(names(paths))

  # remove ignored packages
  ignored <- c(renv_project_ignored_packages(project), renv_packages_base())
  library$Packages <- exclude(library$Packages, ignored)
  packages <- setdiff(packages, ignored)

  # ignore renv when testing
  if (renv_tests_running()) {
    packages <- setdiff(packages, "renv")
    lockfile$Packages[["renv"]] <- NULL
    library$Packages[["renv"]] <- NULL
  }

  synchronized <- all(

    renv_status_check_synchronized(
      project      = project,
      lockfile     = lockfile,
      library      = library,
      packages     = packages
    ),

    if (sources)
      renv_status_check_unknown_sources(project, lockfile)

  )

  if (cache)
    renv_status_check_cache(project)

  if (synchronized)
    vwritef("* The project is already synchronized with the lockfile.")

  list(
    library      = library,
    lockfile     = lockfile,
    synchronized = synchronized
  )

}

renv_status_check_missing_lockfile <- function(project, lockpath) {

  if (file.exists(lockpath))
    return(TRUE)

  text <- if (identical(lockpath, renv_lockfile_path(project)))
    "* This project has not yet been snapshotted -- 'renv.lock' does not exist."
  else
    sprintf("* Lockfile %s does not exist.", renv_path_pretty(lockpath))

  vwritef(text)
  FALSE

}

renv_status_check_missing_library <- function(project, libpaths) {

  projlib <- nth(libpaths, 1L)
  if (file.exists(projlib))
    return(TRUE)

  text <- if (identical(projlib, renv_paths_library(project = project)))
    "* This project's private library is empty or does not exist."
  else
    sprintf("* Library %s is empty or does not exist.", renv_path_pretty(projlib))

  vwritef(text)
  FALSE

}

renv_status_check_unknown_sources <- function(project, lockfile) {
  records <- renv_lockfile_records(lockfile)
  renv_check_unknown_source(records, project)
}

renv_status_check_synchronized <- function(project,
                                           lockfile,
                                           library,
                                           packages)
{
  # flag set to FALSE if any of the below checks report out-of-sync
  ok <- TRUE

  # extract record components
  lockfile <- renv_lockfile_records(lockfile)
  library  <- renv_lockfile_records(library)

  # NOTE: If we have some packages which are required by the project,
  # but are presently not installed, then the 'packages' vector
  # (which tries to enumerate all transitive dependencies) may be
  # incomplete. In this scenario, we need to avoid certain reports which
  # might be misleading to the user.
  missing <- setdiff(packages, c(names(library)))
  if (length(missing)) {

    if (renv_tests_running()) {
      condition <- "renv.status.used_but_not_installed"
      renv_condition_signal(condition, missing)
    }

    lockmsg <- "The following packages are recorded in the lockfile, but not installed:"
    usedmsg <- "The following packages are used in this project, but not installed:"
    restoremsg <- "Use `renv::restore()` to restore the packages recorded in the lockfile."
    installmsg <- "Consider installing these packages -- for example, with `renv::install()`."
    statusmsg <- "Use `renv::status()` afterwards to re-assess the project state."

    # if these packages are in the lockfile, report those records
    if (all(missing %in% names(lockfile))) {

      records <- keep(lockfile, missing)
      renv_pretty_print_records(
        records,
        preamble  = lockmsg,
        postamble = restoremsg
      )

      return(FALSE)

    }

    # otherwise, try to report intelligently
    postamble <- if (any(missing %in% names(lockfile))) {
      c(restoremsg, statusmsg)
    } else {
      c(installmsg, statusmsg)
    }

    renv_pretty_print(
      missing,
      preamble  = usedmsg,
      postamble = postamble
    )

    return(FALSE)

  }


  # - Recorded in the lockfile.
  # - Not installed in the library.
  # - Used in the project.
  #
  # Use `renv::restore()` to install these packages.
  records <- lockfile %>%
    exclude(names(library)) %>%
    keep(packages)

  if (length(records)) {

    if (renv_tests_running()) {
      condition <- "renv.status.recorded_but_not_installed"
      renv_condition_signal(condition, records)
    }

    renv_pretty_print_records(
      records,
      "The following package(s) are recorded in the lockfile, but not installed:",
      "Use `renv::restore()` to install these packages."
    )

    ok <- FALSE

  }

  # Case 3.
  #
  # - Installed in the library.
  # - Not recorded in the lockfile.
  # - Used in the project.
  #
  # Use `renv::snapshot()` to add these packages to the lockfile.
  records <- library %>%
    exclude(names(lockfile)) %>%
    keep(packages)

  if (length(records)) {

    if (renv_tests_running()) {
      condition <- "renv.status.installed_but_not_recorded"
      renv_condition_signal(condition, records)
    }

    renv_pretty_print_records(
      records,
      "The following package(s) are installed, but not recorded in the lockfile:",
      "Use `renv::snapshot()` to add these packages to the lockfile."
    )

    ok <- FALSE

  }

  # Case 5 and 6.
  #
  # - Recorded in the lockfile
  # - Installed (or not?) in the library.
  # - Not used in the project
  #
  # Use renv::snapshot() to remove from the lockfile.
  records <- lockfile %>% exclude(packages)
  if (length(records)) {

    if (renv_tests_running()) {
      condition <- "renv.status.recorded_but_not_used"
      renv_condition_signal(condition, records)
    }

    renv_pretty_print_records(
      records,
      preamble =
        "The following packages are recorded in the lockfile, but do not appear to be used in this project:",
      postamble =
        "Use `renv::snapshot()` if you'd like to remove these packages from the lockfile."
    )

    ok <- FALSE

  }

  # Case 7 and 8.
  #
  # - Not recorded in the lockfile.
  # - Installed (or not?) in the library.
  # - Not used in the project.
  #
  # No action; it's okay if some auxiliary packages are installed.

  # Report about other changes.
  actions <- renv_lockfile_diff_packages(lockfile, library)
  rest <- c("upgrade", "downgrade", "crossgrade")
  if (any(rest %in% actions)) {

    matches <- actions[actions %in% rest]

    rlock <- renv_lockfile_records(lockfile)[names(matches)]
    rlibs <- renv_lockfile_records(library)[names(matches)]

    data <- data.frame(
      "  Package"          = names(matches),
      "  Lockfile Version" = extract_chr(rlock, "Version"),
      "  Library Version"  = extract_chr(rlibs, "Version"),
      row.names            = NULL,
      stringsAsFactors     = FALSE,
      check.names          = FALSE
    )

    writeLines("The following package(s) are out of sync:")
    writeLines("")
    print(data, row.names = FALSE)
    writeLines("")
    writeLines("Use `renv::snapshot()` to save the state of your library to the lockfile.")
    writeLines("Use `renv::restore()` to restore your library from the lockfile.")
    writeLines("")

    ok <- FALSE

  }

  ok
}

renv_status_check_cache <- function(project) {

  if (renv_cache_config_enabled(project = project))
    renv_cache_diagnose()

}

renv_status_signal_unsynchronized <- function() {
  renv_condition_signal("status.unsynchronized", list())
}
