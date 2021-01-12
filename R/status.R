
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
                   cache = FALSE)
{
  renv_scope_error_handler()
  renv_dots_check(...)

  project <- renv_project_resolve(project)
  renv_scope_lock(project = project)

  libpaths <- renv_libpaths_resolve(library)
  lockpath <- lockfile %||% renv_lockfile_path(project)

  invisible(renv_status_impl(project, libpaths, lockpath, cache))
}

renv_status_impl <- function(project, libpaths, lockpath, cache) {

  # check to see if we've initialized this project
  if (!renv_project_initialized(project)) {
    vwritef("* This project has not yet been initialized.")
    return(list())
  }

  libstate <- renv_status_check_missing_library(project, libpaths)
  lockfile <- renv_status_check_missing_lockfile(project, lockpath)

  renv_status_check_synchronized(
    project  = project,
    lockfile = lockfile,
    libpaths = libpaths,
    libstate = libstate
  )

  renv_status_check_unknown_sources(project, lockfile)
  renv_status_check_used_packages(project, lockfile, libstate)

  if (cache)
    renv_status_check_cache(project)

  list(library = libstate, lockfile = lockfile)

}

renv_status_check_missing_lockfile <- function(project, lockpath) {

  if (file.exists(lockpath))
    return(renv_lockfile_read(lockpath))

  if (identical(lockpath, renv_lockfile_path(project))) {
    text <- "* This project has not yet been snapshotted -- 'renv.lock' does not exist."
    vwritef(text)
  } else {
    fmt <- "* Lockfile %s does not exist."
    vwritef(fmt, renv_path_pretty(lockpath))
  }

  list()

}

renv_status_check_missing_library <- function(project, libpaths) {

  projlib <- nth(libpaths, 1L)
  if (file.exists(projlib)) {
    renv_scope_options(renv.verbose = FALSE)
    snapshotted <- snapshot(project  = project,
                            library  = libpaths,
                            lockfile = NULL,
                            force    = TRUE)
    return(snapshotted)
  }

  if (identical(projlib, renv_paths_library(project = project))) {
    text <- "* This project's private library is empty or does not exist."
    vwritef(text)
  } else {
    fmt <- "* Library %s is empty or does not exist."
    vwritef(fmt, renv_paths_project(projlib))
  }

  list()

}

renv_status_check_used_packages <- function(project, lockfile, libstate) {

  # only done when using implicit snapshots in a project
  type <- settings$snapshot.type(project = project)
  if (!type %in% c("implicit", "packrat"))
    return(FALSE)

  deps <- dependencies(project, progress = FALSE)
  used <- sort(unique(deps$Package))

  ignored <- c(
    renv_packages_base(),
    renv_project_ignored_packages(project),
    names(renv_records(lockfile)),
    names(renv_records(libstate))
  )

  missing <- setdiff(used, ignored)
  if (empty(missing))
    return(TRUE)

  renv_pretty_print(
    missing,
    "The following package(s) are used in the project, but are not installed:",
    c(
      "Consider installing these packages, and then using `renv::snapshot()`",
      "to record these packages in the lockfile."
    ),
    wrap = FALSE
  )

  if (renv_tests_running())
    renv_condition_signal("renv.status.used_but_not_installed", missing)

  FALSE

}

renv_status_check_unknown_sources <- function(project, lockfile) {
  records <- renv_records(lockfile)
  renv_check_unknown_source(records, project)
}

renv_status_check_synchronized <- function(project,
                                           lockfile,
                                           libpaths,
                                           libstate)
{
  # diff packages
  actions <- renv_lockfile_diff_packages(lockfile, libstate)
  if (empty(actions)) {
    vwritef("* The project is already synchronized with the lockfile.")
    return(TRUE)
  }

  if ("install" %in% actions) {

    records <- renv_records_select(libstate, actions, "install")

    if (renv_tests_running()) {
      condition <- "renv.status.installed_but_not_recorded"
      renv_condition_signal(condition, records)
    }

    renv_pretty_print_records(
      records,
      "The following package(s) are installed but not recorded in the lockfile:",
      "Use `renv::snapshot()` to add these packages to your lockfile."
    )

  }

  if ("remove" %in% actions) {

    # we need to differentiate between packages that are 'removed' because they
    # are no longer used in the project, versus packages which are simply not
    # installed in the project library anymore
    records <- renv_records_select(lockfile, actions, "remove")

    if (settings$snapshot.type() %in% c("implicit", "packrat")) {

      deps <- dependencies(project, progress = FALSE)
      pkgpaths <- renv_package_dependencies(
        packages = unique(deps$Package),
        project  = project,
        libpaths = libpaths
      )

      used <- intersect(names(records), names(pkgpaths))
      unused <- setdiff(names(records), used)

      if (renv_tests_running()) {
        condition <- "renv.status.recorded_but_no_longer_used"
        renv_condition_signal(condition, records[unused])
      }

      renv_pretty_print_records(
        records[unused],
        "The following package(s) are no longer used in this project:",
        "Use `renv::snapshot()` to remove them from the lockfile."
      )

      records <- records[used]

    }

    if (renv_tests_running()) {
      condition <- "renv.status.recorded_but_not_installed"
      renv_condition_signal(condition, records)
    }

    renv_pretty_print_records(
      records,
      "The following package(s) are recorded in the lockfile but not installed:",
      "Use `renv::restore()` to install these packages."
    )
  }

  rest <- c("upgrade", "downgrade", "crossgrade")
  if (any(rest %in% actions)) {

    matches <- actions[actions %in% rest]

    rlock <- renv_records(lockfile)[names(matches)]
    rlibs <- renv_records(libstate)[names(matches)]

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
  }

  FALSE
}

renv_status_check_cache <- function(project) {

  if (renv_cache_config_enabled(project = project))
    renv_cache_diagnose()

}
