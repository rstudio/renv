
#' Hydrate a Project
#'
#' Discover the \R packages used within a project, and then install those
#' packages into the active library. This effectively allows you to fork the
#' state of your default \R libraries for use within a project library.
#'
#' It may occasionally be useful to use `renv::hydrate()` to update the packages
#' used within a project that has already been initialized. However, be aware
#' that it's possible that the packages pulled in may not actually be compatible
#' with the packages already installed in the project library, so you should
#' exercise caution when doing so.
#'
#' @section Sources:
#'
#' `hydrate()` attempts to re-use packages already installed on your system,
#' to avoid unnecessary attempts to download and install packages from remote
#' sources. When `NULL` (the default), `hydrate()` will attempt to discover \R
#' packages from the following sources (in order):
#'
#' - The user library,
#' - The site library,
#' - The system library,
#' - The `renv` cache.
#'
#' If package is discovered in one of these locations, `renv` will attempt to
#' copy or link that package into the requested library as appropriate.
#'
#' @section Missing Packages:
#'
#' If `renv` discovers that your project depends on \R packages not currently
#' installed in your user library, then it will attempt to install those
#' packages from the active R repositories.
#'
#' @inherit renv-params
#'
#' @param packages The set of \R packages to install. When `NULL`, the
#'   set of packages as reported by [dependencies()] is used.
#'
#' @param library The \R library to be hydrated. When `NULL`, the active
#'   library as reported by `.libPaths()` is used.
#'
#' @param update Boolean; should `hydrate()` attempt to update already-installed
#'   packages if the requested package is already installed in the project
#'   library? Set this to `"all"` if you'd like _all_ packages to be refreshed
#'   from the source library if possible.
#'
#' @param sources A set of library paths from which `renv` should attempt to
#'   draw packages. See **Sources** for more details.
#'
#' @param prompt Boolean; prompt the user before taking any action? Ignored
#'   when `report = FALSE`.
#'
#' @param report Boolean; display a report of what packages will be installed
#'   by `renv::hydrate()`?
#'
#' @return A named \R list, giving the packages that were used for hydration
#'   as well as the set of packages which were not found.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # hydrate the active library
#' renv::hydrate()
#'
#' }
hydrate <- function(packages = NULL,
                    ...,
                    library = NULL,
                    update  = FALSE,
                    sources = NULL,
                    prompt  = interactive(),
                    report  = TRUE,
                    project = NULL)
{
  renv_scope_error_handler()
  renv_dots_check(...)

  project <- renv_project_resolve(project)
  renv_scope_lock(project = project)

  renv_activate_prompt("hydrate", library, prompt, project)

  library <- renv_path_normalize(library %||% renv_libpaths_default())
  packages <- packages %||% renv_hydrate_packages(project)

  # find packages used in this project, and the dependencies of those packages
  deps <- renv_hydrate_dependencies(project, packages, sources)

  # remove 'renv' since it's managed separately
  deps$renv <- NULL

  # remove base + missing packages
  base <- renv_packages_base()
  na <- deps[is.na(deps)]
  packages <- deps[renv_vector_diff(names(deps), c(names(na), base))]

  # figure out if we will copy or link
  linkable <- renv_cache_linkable(project = project, library = library)

  # get and construct path to library
  ensure_directory(library)

  # only hydrate with packages that are either not currently installed,
  # or (if update = TRUE) the version in the library is newer
  packages <- renv_hydrate_filter(packages, library, update)

  # inform user about changes
  if (report) {
    renv_hydrate_report(packages, na, linkable)
    if (length(packages) || length(na)) {
      if (prompt && !proceed()) {
        renv_report_user_cancel()
        invokeRestart("abort")
      }
    }
  }

  # check for nothing to be done
  if (empty(packages) && empty(na)) {
    if (report)
      vwritef("* No new packages were discovered in this project; nothing to do.")
    return(invisible(list(packages = list(), missing = list())))
  }

  # copy packages from user library to cache
  before <- Sys.time()
  if (length(packages)) {
    if (linkable)
      renv_hydrate_link_packages(packages, library, project)
    else
      renv_hydrate_copy_packages(packages, library, project)
  }
  after <- Sys.time()

  if (report) {
    time <- difftime(after, before, units = "auto")
    fmt <- "* Hydrated %s packages in %s."
    vwritef(fmt, length(packages), renv_difftime_format(time))
  }

  # attempt to install missing packages (if any)
  missing <- renv_hydrate_resolve_missing(project, library, na)

  # we're done!
  result <- list(packages = packages, missing = missing)
  invisible(result)
}

renv_hydrate_filter <- function(packages, library, update) {

  # run filter
  keep <- enumerate(
    packages,
    renv_hydrate_filter_impl,
    library = library,
    update = update,
    FUN.VALUE = logical(1)
  )

  # filter based on kept packages
  packages[keep]

}

renv_hydrate_filter_impl <- function(package, path, library, update) {

  # if user has requested hydration of all packages, respect that
  if (identical(update, "all"))
    return(TRUE)

  # is the package already installed in the requested library?
  # if not, then we'll want to hydrate this package
  # if so, we'll want to compare the version first and
  # hydrate only if the requested version is newer than the current
  descpath <- file.path(library, package, "DESCRIPTION")
  desc <- catch(renv_description_read(path = descpath))
  if (inherits(desc, "error"))
    return(TRUE)

  # get the current package version
  current <- catch(numeric_version(desc[["Version"]]))
  if (inherits(current, "error"))
    return(TRUE)

  # if the package is already installed and we're not updating, stop here
  if (identical(update, FALSE))
    return(FALSE)

  # check to-be-copied package version
  requested <- catch({
    desc <- renv_description_read(path = path)
    numeric_version(desc[["Version"]])
  })

  # only hydrate with a newer version
  requested > current

}

renv_hydrate_packages_rprofile <- function() {

  enabled <-
    identical(config$user.profile(), TRUE) &&
    !renv_tests_running()

  if (!enabled)
    return()

  rprofile <- Sys.getenv("R_PROFILE_USER", unset = "~/.Rprofile")
  if (!file.exists(rprofile))
    return()

  dependencies(rprofile, quiet = TRUE, dev = TRUE)

}

renv_hydrate_packages <- function(project) {

  deps <- dependencies(project, quiet = TRUE, dev = TRUE)

  profdeps <- renv_hydrate_packages_rprofile()
  if (length(deps) && length(profdeps))
    deps <- bind(list(deps, profdeps))

  unique(deps$Package)

}

renv_hydrate_dependencies <- function(project,
                                      packages = NULL,
                                      libpaths = NULL)
{
  vprintf("* Discovering package dependencies ... ")
  ignored <- renv_project_ignored_packages(project = project)
  packages <- renv_vector_diff(packages, ignored)
  libpaths <- libpaths %||% renv_hydrate_libpaths()
  all <- renv_package_dependencies(packages, project = project, libpaths = libpaths)
  vwritef("Done!")

  all
}

# NOTE: we don't want to look in user / site libraries when testing
# on CRAN, as we may accidentally find versions of packages available
# on CRAN but not that we want to use during tests
renv_hydrate_libpaths <- function() {

  conf <- config$hydrate.libpaths()
  if (is.character(conf) && length(conf))
    conf <- unlist(strsplit(conf, ":", fixed = TRUE))

  libpaths <- if (renv_tests_running())
    renv_libpaths_all()
  else if (length(conf))
    conf
  else
    c(renv_libpaths_user(), renv_libpaths_site(), renv_libpaths_system())

  libpaths <- .expand_R_libs_env_var(libpaths)
  normalizePath(libpaths, winslash = "/", mustWork = FALSE)

}

# takes a package called 'package' installed at location 'location',
# copies that package into the cache, and then links from the cache
# to the (private) library 'library'
renv_hydrate_link_package <- function(package, location, library) {

  # construct path to cache
  record <- renv_snapshot_description(location)
  cache <- renv_cache_find(record)
  if (!nzchar(cache))
    return(FALSE)

  # copy package into the cache
  if (!file.exists(cache)) {
    ensure_parent_directory(cache)
    renv_file_copy(location, cache)
  }

  # link package back from cache to library
  target <- file.path(library, package)
  ensure_parent_directory(target)
  renv_file_link(cache, target, overwrite = TRUE)

}

renv_hydrate_link_packages <- function(packages, library, project) {

  header <- if (renv_path_same(library, renv_paths_library(project = project)))
    "* Linking packages into the project library ... "
  else
    sprintf("* Linking packages into %s ... ", renv_path_pretty(library))

  vprintf(header)
  callback <- renv_progress_callback(renv_hydrate_link_package, length(packages))
  cached <- enumerate(packages, callback, library = library)
  vwritef("Done!")
  cached

}

# takes a package called 'package' installed at location 'location',
# and copies it to the library 'library'
renv_hydrate_copy_package <- function(package, location, library) {
  target <- file.path(library, package)
  renv_file_copy(location, target, overwrite = TRUE)
}

renv_hydrate_copy_packages <- function(packages, library, project) {

  header <- if (renv_path_same(library, renv_paths_library(project = project)))
    "* Copying packages into the project library ... "
  else
    sprintf("* Copying packages into %s ... ", renv_path_pretty(library))

  vprintf(header)
  callback <- renv_progress_callback(renv_hydrate_copy_package, length(packages))
  copied <- enumerate(packages, callback, library = library)
  vwritef("Done!")
  copied
}

renv_hydrate_resolve_missing <- function(project, library, na) {

  # make sure requested library is made active
  ensure_directory(library)
  renv_scope_libpaths(unique(c(library, .libPaths())))

  # figure out which packages are missing (if any)
  packages <- names(na)
  installed <- renv_installed_packages()
  if (all(packages %in% installed$Package))
    return()

  vwritef("* Resolving missing dependencies  ... ")

  # define a custom error handler for packages which
  # we failed to retrieve
  errors <- stack()
  handler <- function(package, action) {
    error <- catch(action)
    if (inherits(error, "error"))
      errors$push(list(package = package, error = error))
  }

  # perform the restore
  renv_scope_restore(
    project  = project,
    library  = library,
    packages = packages,
    handler  = handler
  )

  records <- retrieve(packages)
  renv_install_impl(records)

  # if we failed to restore anything, warn the user
  data <- errors$data()
  if (empty(data))
    return()

  if (renv_verbose()) {

    text <- map_chr(data, function(item) {
      package <- item$package
      message <- conditionMessage(item$error)
      short <- trunc(paste(message, collapse = ";"), 60L)
      sprintf("[%s]: %s", package, short)
    })

    renv_pretty_print(
      text,
      "The following package(s) were not installed successfully:",
      "You may need to manually download and install these packages.",
      wrap = FALSE
    )

  }

  invisible(data)

}

renv_hydrate_report <- function(packages, na, linkable) {

  if (renv_bootstrap_tests_running())
    return()

  if (length(packages)) {

    # this is mostly a hacky way to get a list of records that the existing
    # record pretty-printer can handle in a clean way
    records <- enumerate(packages, function(package, library) {
      descpath <- file.path(library, "DESCRIPTION")
      record <- renv_snapshot_description(descpath)
      record$Repository <- NULL
      record$Source <- renv_path_aliased(dirname(library))
      record
    })

    preamble <- "The following packages were discovered:"
    postamble <- sprintf(
      "They will be %s into the project library.",
      if (linkable) "linked" else "copied"
    )

    formatter <- function(lhs, rhs) {
      renv_record_format_short(rhs, versioned = TRUE)
    }

    renv_pretty_print_records_pair(
      old = list(),
      new = records,
      preamble  = preamble,
      postamble = postamble,
      formatter = formatter
    )

  }

  if (length(na)) {
    renv_pretty_print(
      values    = csort(names(na)),
      preamble  = "The following packages are used in this project, but not available locally:",
      postamble = "renv will attempt to download and install these packages."
    )
  }

}
