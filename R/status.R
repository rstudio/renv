
the$status_running <- FALSE

#' Report inconsistencies between lockfile, library, and dependencies
#'
#' @description
#' `renv::status()` reports issues caused by inconsistencies across the project
#' lockfile, library, and [dependencies()]. In general, you should strive to
#' ensure that `status()` reports no issues, as this maximises your chances of
#' successfully `restore()`ing the project in the future or on another machine.
#'
#' `renv::load()` will report if any issues are detected when starting an
#' renv project; we recommend resolving these issues before doing any
#' further work on your project.
#'
#' See the headings below for specific advice on resolving any issues
#' revealed by `status()`.
#'
#' # Missing packages
#'
#' First, ensure that all packages used by the project are installed. This is
#' important to do first because if any packages are missing we can't tell for
#' sure that a package isn't used; it might be a dependency that we don’t know
#' about.
#'
#' There are four possibilities for an uninstalled package:
#'
#' * If it’s used and recorded, call `renv::restore()` to install the version
#'   specified in the lockfile.
#' * If it’s used and not recorded, call `renv::install()` to install it
#'   from CRAN or elsewhere.
#' * If it’s not used and recorded, call `renv::snapshot()` to
#'   remove it from the lockfile.
#' * If it's not used and not recorded, there's nothing to do. This the most
#'   common state because you only use a small fraction of all available
#'   packages in any one project.
#'
#' If you have multiple packages in an inconsistent state, we recommend
#' `renv::restore()`, then `renv::install()`, then `renv::snapshot()`, but
#' that also suggests you should be running status more frequently.
#'
#' # Lockfile vs `dependencies()`
#'
#' Next we need to ensure that packages are recorded in the lockfile if and
#' only if they are used by the project. Fixing issues of this nature only
#' requires calling  `snapshot()` because there are four possibilities for
#' a package:
#'
#' * If it’s used and recorded, it’s ok.
#' * If it’s used and not recorded, call `renv::snapshot()` to add it to the
#'   lockfile.
#' * If it’s not used but is recorded, call `renv::snapshot()` to remove
#'   it from the lockfile.
#' * If it’s not used and not recorded, it’s also ok, as it may be a
#'   development dependency.
#'
#' # Out-of-sync sources
#'
#' The final issue to resolve is any inconsistencies between the version of
#' the package recorded in the lockfile and the version installed in your
#' library. To fix these issues you'll need to either call `renv::restore()`
#' or `renv::snapshot()`:
#'
#' * Call `renv::snapshot()` if your project code is working. This implies that
#'   the library is correct and you need to update your lockfile.
#' * Call `renv::restore()` if your project code isn't working. This probably
#'   implies that you have the wrong package versions installed and you need
#'   to restore from known good state in the lockfile.
#'
#' If you're not sure which case applies, it's generally safer to call
#' `renv::snapshot()`. If you want to rollback to an earlier known good
#' status, see [renv::history()] and [renv::revert()].
#'
#' @inherit renv-params
#'
#' @param library The library paths. By default, the library paths associated
#'   with the requested project are used.
#'
#' @param sources Boolean; check that each of the recorded packages have a
#'   known installation source? If a package has an unknown source, renv
#'   may be unable to restore it.
#'
#' @param cache Boolean; perform diagnostics on the global package cache?
#'   When `TRUE`, renv will validate that the packages installed into the
#'   cache are installed at the expected + proper locations, and validate the
#'   hashes used for those storage locations.
#'
#' @return This function is normally called for its side effects, but
#'   it invisibly returns a list containing the following components:
#'
#'   * `library`: packages in your library.
#'   * `lockfile`: packages in the lockfile.
#'   * `synchronized`: are the library and lockfile in sync?
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

renv_status_check_initialized <- function(project, lockpath = NULL) {

  projlib <- renv_paths_library(project = project)
  lockpath <- lockpath %||% renv_paths_lockfile(project = project)

  haslib <- file.exists(projlib)
  haslock <- file.exists(lockpath)

  if (haslib && haslock)
    return(TRUE)

  if (haslib && !haslock) {
    writef(c(
      "This project does not contain a lockfile.",
      "Use renv::snapshot() to create a lockfile."
    ))
  } else if (!haslib && haslock) {
    writef(c(
      "There are no packages installed in the project library.",
      "Use renv::restore() to install the packages defined in lockfile."
    ))
  } else {
    writef(c(
      "This project does not appear to be using renv.",
      "Use renv::init() to initialize the project."
    ))
  }

  FALSE

}

renv_status_impl <- function(project, libpaths, lockpath, sources, cache) {

  # mark status as running
  the$status_running <- TRUE
  defer(the$status_running <- FALSE)

  # check to see if we've initialized this project
  if (!renv_status_check_initialized(project, lockpath)) {
    return(list(
      library = list(Packages = named(list())),
      lockfile = list(Packages = named(list())),
      synchronized = FALSE
    ))
  }

  # get all dependencies, including transitive
  dependencies <- renv_snapshot_dependencies(project, dev = FALSE)
  packages <- sort(union(dependencies, "renv"))
  paths <- renv_package_dependencies(packages, libpaths = libpaths, project = project)
  packages <- as.character(names(paths))

  # read project lockfile
  lockfile <- renv_lockfile_read(lockpath)

  # get lockfile capturing current library state
  library <- renv_lockfile_create(
    libpaths = libpaths,
    type     = "all",
    prompt   = FALSE,
    project  = project
  )

  # remove ignored packages
  ignored <- c(
    renv_project_ignored_packages(project),
    renv_packages_base(),
    if (renv_tests_running()) "renv"
  )

  packages <- setdiff(packages, ignored)
  renv_lockfile_records(lockfile) <- exclude(renv_lockfile_records(lockfile), ignored)
  renv_lockfile_records(library) <- exclude(renv_lockfile_records(library), ignored)

  synchronized <-
    renv_status_check_consistent(lockfile, library, packages) &&
    renv_status_check_synchronized(lockfile, library)

  if (sources) {
    synchronized <- synchronized &&
      renv_status_check_unknown_sources(project, lockfile)
  }

  if (cache)
    renv_status_check_cache(project)

  if (synchronized)
    writef("No issues found -- the project is in a consistent state.")
  else
    writef(c("", "See ?renv::status() for advice on resolving these issues."))

  list(
    library      = library,
    lockfile     = lockfile,
    synchronized = synchronized
  )

}

renv_status_check_unknown_sources <- function(project, lockfile) {
  renv_check_unknown_source(renv_lockfile_records(lockfile), project)
}

renv_status_check_consistent <- function(lockfile, library, used) {

  lockfile <- renv_lockfile_records(lockfile)
  library <- renv_lockfile_records(library)

  packages <- sort(unique(c(names(library), names(lockfile), used)))

  status <- data.frame(
    package = packages,
    installed = packages %in% names(library),
    recorded = packages %in% names(lockfile),
    used = packages %in% used
  )

  pkg_ok <- status$installed & (status$used == status$recorded)
  if (all(pkg_ok)) {
    return(TRUE)
  }

  if (renv_verbose()) {
    # If any packages are not installed, we don't know for sure what's used
    # because our dependency graph is incomplete
    missing <- any(!status$installed)

    issues <- status[!pkg_ok, , drop = FALSE]
    issues$installed <- ifelse(issues$installed, "y", "n")
    issues$recorded <- ifelse(issues$recorded, "y", "n")
    issues$used <- ifelse(issues$used, "y", if (missing) "?" else "n")

    writef("The following package(s) are in an inconsistent state:")
    writef()
    print(issues, row.names = FALSE, right = FALSE)
  }

  FALSE

}

renv_status_check_synchronized <- function(lockfile, library) {

  lockfile <- renv_lockfile_records(lockfile)
  library <- renv_lockfile_records(library)

  actions <- renv_lockfile_diff_packages(lockfile, library)
  rest <- c("upgrade", "downgrade", "crossgrade")

  if (all(!rest %in% actions)) {
    return(TRUE)
  }

  pkgs <- names(actions[actions %in% rest])
  renv_pretty_print_records_pair(
    preamble = "The following package(s) are out of sync [lockfile -> library]:",
    lockfile[pkgs],
    library[pkgs],
  )

  FALSE

}

renv_status_check_cache <- function(project) {

  if (renv_cache_config_enabled(project = project))
    renv_cache_diagnose()

}

