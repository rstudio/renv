
the$status_running <- FALSE

#' Report differences between lockfile and project library
#'
#' `status()` reports on problems cauased by inconsistencies across the project
#' lockfile, library, and [dependencies()]. In general, you should strive to
#' ensure that `status()` reports no problems, as this maximises your chances
#' of succesfully `restore()`ing the project in the future or on another
#' machine.  See the headings below for specific advice on resolving any
#' problems revealed by `status()`.
#'
#' # Missing packages
#'
#' First, ensure that all packages used by the project are installed. This is
#' important do to first because if any packages are missing we can’t tell for
#' sure that a package isn’t used; it might be a dependency that we don’t know
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
#' # Lockfile vs `dependencies()`
#'
#' Next we need to ensure that packages are recorded in the lockfile if and
#' only if they are used by the project. Fixing problems of this nature only
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
#' # Inconsistent sources
#'
#' The final problem to resolve is any inconsistencies between packages
#' recorded in the lockfile and installed in your library. To fix these
#' problems you'll need to either call `renv::restore()` or `renv::snapshot()`:
#'
#' * Call `renv::snapshot()` if your project code is working. This implies that
#'   the library is correct and you need to update your lockfile.
#' * Call `renv::restore()` if your project code isn't working. This probably
#'   implies that you have the wrong package versions installed and you need
#'   to restore from known good state in the lockfile.
#'
#' If you're not sure which case applies, it's generally safer to call
#' `renv::snapshot()`
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

renv_status_impl <- function(project, libpaths, lockpath, sources, cache) {

  default <- list(library = list(), lockfile = list(), synchronized = FALSE)

  # check to see if we've initialized this project
  if (!renv_project_initialized(project)) {
    writef("* This project has not yet been initialized.")
    return(default)
  }

  # mark status as running
  the$status_running <- TRUE
  defer(the$status_running <- FALSE)

  # check for existing lockfile, library
  ok <-
    renv_status_check_missing_library(project, libpaths) &&
    renv_status_check_missing_lockfile(project, lockpath)

  if (!ok)
    return(default)

  # get lockfile records
  lockfile <- renv_lockfile_records(renv_lockfile_read(lockpath))

  # get library records
  library <- renv_snapshot_libpaths(libpaths = libpaths, project = project)

  # get all dependencies, including transitive
  dependencies <- renv_snapshot_dependencies(project, dev = FALSE)
  packages <- sort(union(dependencies, "renv"))
  paths <- renv_package_dependencies(packages, project = project)
  packages <- as.character(names(paths))
  # projects will implicitly depend on BiocManager & BiocVersion if any
  # Bioconductor packages are in use
  pkg_sources <- extract_chr(keep(library, packages), "Source")
  if ("Bioconductor" %in% pkg_sources)
    packages <- union(packages, renv_bioconductor_manager())

  # remove ignored packages
  ignored <- c(
    renv_project_ignored_packages(project),
    renv_packages_base(),
    if (renv_tests_running()) "renv"
  )
  packages <- setdiff(packages, ignored)
  lockfile <- exclude(lockfile, ignored)
  library <- exclude(library, ignored)

  synchronized <- renv_status_check_synchronized(
    project      = project,
    lockfile     = lockfile,
    library      = library,
    used         = packages
  )

  if (sources) {
    synchronized <- synchronized &&
      renv_status_check_unknown_sources(project, lockfile)
  }

  if (cache)
    renv_status_check_cache(project)

  if (synchronized)
    writef("* The project is already synchronized with the lockfile.")

  list(
    library      = library,
    lockfile     = lockfile,
    synchronized = synchronized
  )

}

renv_status_check_missing_lockfile <- function(project, lockpath) {

  if (file.exists(lockpath))
    return(TRUE)

  if (identical(lockpath, renv_lockfile_path(project)))
    writef("* This project has not yet been snapshotted -- 'renv.lock' does not exist.")
  else
    writef("* Lockfile %s does not exist.", renv_path_pretty(lockpath))

  FALSE

}

renv_status_check_missing_library <- function(project, libpaths) {

  projlib <- nth(libpaths, 1L)
  if (file.exists(projlib))
    return(TRUE)

  if (identical(projlib, renv_paths_library(project = project)))
    writef("* This project's private library is empty or does not exist.")
  else
    writef("* Library %s is empty or does not exist.", renv_path_pretty(projlib))

  FALSE

}

renv_status_check_unknown_sources <- function(project, lockfile) {
  renv_check_unknown_source(lockfile, project)
}

renv_status_check_synchronized <- function(project,
                                           lockfile,
                                           library,
                                           used)
  {


  packages <- sort(unique(c(names(library), names(lockfile), used)))

  status <- data.frame(
    package = packages,
    installed = packages %in% names(library),
    recorded = packages %in% names(lockfile),
    used = packages %in% used
  )

  pkg_ok <- status$installed & (status$used == status$recorded)
  ok <- all(pkg_ok)
  if (!ok && renv_verbose()) {

    problems <- status[!pkg_ok, , drop = FALSE]
    # If any packages are not installed, we don't know for sure what's used
    # because our dependency graph is incomplete
    missing <- any(!status$installed)
    status$used <- ifelse(status$used, "y", if (missing) "?" else "n")

    status$installed <- ifelse(status$installed, "y", "n")
    status$recorded <- ifelse(status$recorded, "y", "n")

    writef("The following packages are out of sync:")
    writef()
    print(status[!pkg_ok, , drop = FALSE], row.names = FALSE, right = FALSE)
    writef()
    writef("See ?status() for advice")
  }

  # other changes, i.e. different version/source -------------------------------
  actions <- renv_lockfile_diff_packages(library, lockfile)
  rest <- c("upgrade", "downgrade", "crossgrade")

  if (any(rest %in% actions)) {

    matches <- actions[actions %in% rest]

    rlock <- renv_lockfile_records(lockfile)[names(matches)]
    rlibs <- renv_lockfile_records(library)[names(matches)]

    renv_pretty_print_records_pair(
      rlock,
      rlibs,
      preamble = "The following package(s) are out of sync [lockfile -> library]:",
      postamble = c(
        "Use `renv::snapshot()` to save the state of your library to the lockfile.",
        "Use `renv::restore()` to restore your library from the lockfile."
      )
    )

    ok <- FALSE

  }

  ok
}

renv_status_check_cache <- function(project) {

  if (renv_cache_config_enabled(project = project))
    renv_cache_diagnose()

}

