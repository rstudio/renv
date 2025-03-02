
the$status_running <- FALSE

#' Report inconsistencies between lockfile, library, and dependencies
#'
#' @description
#' `renv::status()` reports issues caused by inconsistencies across the project
#' lockfile, library, and [dependencies()]. In general, you should strive to
#' ensure that `status()` reports no issues, as this maximizes your chances of
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
#' `status()` first checks that all packages used by the project are installed.
#' This must be done first because if any packages are missing we can't tell for
#' sure that a package isn't used; it might be a dependency that we don't know
#' about. Once you have resolve any installation issues, you'll need to run
#' `status()` again to reveal the next set of potential problems.
#'
#' There are four possibilities for an uninstalled package:
#'
#' * If it's used and recorded, call `renv::restore()` to install the version
#'   specified in the lockfile.
#' * If it's used and not recorded, call `renv::install()` to install it
#'   from CRAN or elsewhere.
#' * If it's not used and recorded, call `renv::snapshot()` to
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
#' * If it's used and recorded, it's ok.
#' * If it's used and not recorded, call `renv::snapshot()` to add it to the
#'   lockfile.
#' * If it's not used but is recorded, call `renv::snapshot()` to remove
#'   it from the lockfile.
#' * If it's not used and not recorded, it's also ok, as it may be a
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
#' # Different R Version
#'
#' renv will also notify you if the version of R used when the lockfile was
#' generated, and the version of R currently in use, do not match. In this
#' scenario, you'll need to consider:
#'
#' - Is the version of R recorded in the lockfile correct? If so, you'll want
#'   to ensure that version of R is installed and used when working in this
#'   project.
#'
#' - Otherwise, you can call `renv::snapshot()` to update the version of R
#'   recorded in the lockfile, to match the version of R currently in use.
#'
#' If you'd like to set the version of R recorded in a lockfile independently
#' of the version of R currently in use, you can set the `r.version` project
#' setting -- see [settings] for more details.
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
#' @inheritParams dependencies
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
                   cache = FALSE,
                   dev = FALSE)
{
  renv_scope_error_handler()
  renv_dots_check(...)

  renv_snapshot_auto_suppress_next()
  renv_scope_options(renv.prompt.enabled = FALSE)

  the$status_running <- TRUE
  defer(the$status_running <- FALSE)

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  # check to see if we've initialized this project
  if (!renv_status_check_initialized(project, library, lockfile)) {
    result <- list(
      library = list(Packages = named(list())),
      lockfile = list(Packages = named(list())),
      synchronized = FALSE
    )
    return(invisible(result))
  }

  libpaths <- library %||% renv_libpaths_resolve()
  lockpath <- lockfile %||% renv_paths_lockfile(project = project)

  # get all dependencies, including transitive
  dependencies <- renv_snapshot_dependencies(project, dev = dev)
  packages <- sort(union(dependencies, "renv"))
  paths <- renv_package_dependencies(packages, libpaths = libpaths, project = project)
  packages <- as.character(names(paths))

  # read project lockfile
  lockfile <- if (file.exists(lockpath))
    renv_lockfile_read(lockpath)
  else
    renv_lockfile_init(project = project)

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
  renv_lockfile_records(lockfile) <- omit(renv_lockfile_records(lockfile), ignored)
  renv_lockfile_records(library) <- omit(renv_lockfile_records(library), ignored)

  synchronized <- all(
    renv_status_check_consistent(lockfile, library, packages),
    renv_status_check_synchronized(lockfile, library),
    renv_status_check_version(lockfile)
  )

  if (sources) {
    synchronized <- synchronized &&
      renv_status_check_unknown_sources(project, lockfile)
  }

  if (cache)
    renv_status_check_cache(project)

  if (synchronized)
    writef("No issues found -- the project is in a consistent state.")
  else
    writef("See `?renv::status` for advice on resolving these issues.")

  result <- list(
    library      = library,
    lockfile     = lockfile,
    synchronized = synchronized
  )

  invisible(result)

}

renv_status_check_unknown_sources <- function(project, lockfile) {
  renv_check_unknown_source(renv_lockfile_records(lockfile), project)
}

renv_status_check_consistent <- function(lockfile, library, used) {

  lockfile <- renv_lockfile_records(lockfile)
  library <- renv_lockfile_records(library)

  packages <- sort(unique(c(names(library), names(lockfile), used)))

  status <- data_frame(
    package   = packages,
    installed = packages %in% names(library),
    recorded  = packages %in% names(lockfile),
    used      = packages %in% used
  )

  ok <- status$installed & (status$used == status$recorded)
  if (all(ok))
    return(TRUE)

  if (!renv_verbose())
    return(FALSE)

  issues <- status[!ok, , drop = FALSE]
  missing <- issues$used & !issues$installed
  if (all(missing)) {

    bulletin(
      preamble = "The following package(s) are used in this project, but are not installed:",
      values   = issues$package[missing]
    )

    return(FALSE)

  }

  issues$installed <- ifelse(issues$installed, "y", "n")
  issues$recorded <- ifelse(issues$recorded, "y", "n")
  issues$used <- ifelse(issues$used, "y", if (any(missing)) "?" else "n")

  preamble <- "The following package(s) are in an inconsistent state:"

  writef(preamble)
  writef()
  print(issues, row.names = FALSE, right = FALSE)
  writef()

  FALSE

}

renv_status_check_initialized <- function(project, library = NULL, lockfile = NULL) {

  # only done if library and lockfile are NULL; that is, if the user
  # is calling `renv::status()` without arguments
  if (!is.null(library) || !is.null(lockfile))
    return(TRUE)

  # resolve paths to lockfile, primary library path
  library  <- library  %||% renv_paths_library(project = project)
  lockfile <- lockfile %||% renv_paths_lockfile(project = project)

  # check whether the lockfile + library exist
  haslib  <- all(file.exists(library))
  haslock <- file.exists(lockfile)
  if (haslib && haslock)
    return(TRUE)

  # TODO: what about the case where the library exists but no packages are installed?
  # TODO: should this check for an 'renv/activate.R' script?
  # TODO: what if a different project is loaded?
  if (haslib && !haslock) {
    writef(c(
      "This project does not contain a lockfile.",
      "Use `renv::snapshot()` to create a lockfile."
    ))
  } else if (!haslib && haslock) {
    writef(c(
      "There are no packages installed in the project library.",
      "Use `renv::restore()` to install the packages defined in lockfile."
    ))
  } else {
    writef(c(
      "This project does not appear to be using renv.",
      "Use `renv::init()` to initialize the project."
    ))
  }

  FALSE

}

renv_status_check_synchronized <- function(lockfile, library) {

  lockfile <- renv_lockfile_records(lockfile)
  library <- renv_lockfile_records(library)

  actions <- renv_lockfile_diff_packages(lockfile, library)
  rest <- c("upgrade", "downgrade", "crossgrade")

  if (all(!rest %in% actions))
    return(TRUE)

  pkgs <- names(actions[actions %in% rest])
  formatter <- function(lhs, rhs)
    renv_record_format_pair(lhs, rhs, separator = "!=")

  renv_pretty_print_records_pair(
    preamble = "The following package(s) are out of sync [lockfile != library]:",
    old = lockfile[pkgs],
    new = library[pkgs],
    formatter = formatter
  )

  FALSE

}

renv_status_check_version <- function(lockfile) {

  version <- lockfile$R$Version
  if (renv_version_eq(version, getRversion(), n = 2L))
    return(TRUE)

  fmt <- "The lockfile was generated with R %s, but you're using R %s."
  writef(fmt, version, getRversion())
  writef()

  FALSE

}

renv_status_check_cache <- function(project) {

  if (renv_cache_config_enabled(project = project))
    renv_cache_diagnose()

}

