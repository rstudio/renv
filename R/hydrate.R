
#' Hydrate a Project
#'
#' Discover the \R packages used within a project, and then install those
#' packages into the active library. This effectively allows you to fork the
#' state of your default \R libraries for use within a project library.
#'
#' It may occasionally be useful to use `renv::hydrate()` to update the packages
#' used within a project that has already been initialized. However, be warned
#' that it's possible that the packages pulled in may not actually be compatible
#' with the packages installed in the project library, so you should exercise
#' caution when doing so.
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
                    project = NULL)
{
  renv_scope_error_handler()
  renv_dots_check(...)

  project <- renv_project_resolve(project)
  renv_scope_lock(project = project)

  library <- renv_path_normalize(library %||% renv_libpaths_default())
  packages <- packages %||% renv_hydrate_packages(project, sources)

  # find packages used in this project, and the dependencies of those packages
  deps <- renv_hydrate_dependencies(project, packages, sources)

  # remove 'renv' since it's managed separately
  deps$renv <- NULL

  # remove base + missing packages
  base <- renv_packages_base()
  na <- deps[is.na(deps)]
  packages <- deps[renv_vector_diff(names(deps), c(names(na), base))]

  # get and construct path to library
  ensure_directory(library)

  # only hydrate with packages that are either not currently installed,
  # or (if update = TRUE) the version in the library is newer
  packages <- renv_hydrate_filter(packages, library, update)

  # copy packages from user library to cache
  linkable <- renv_cache_linkable(project = project, library = library)
  if (linkable)
    renv_hydrate_link_packages(packages, library)
  else
    renv_hydrate_copy_packages(packages, library)

  # attempt to install missing packages (if any)
  missing <- renv_hydrate_resolve_missing(project, na)

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

renv_hydrate_packages <- function(project, libpaths = NULL) {

  deps <- dependencies(project, quiet = TRUE, dev = TRUE)

  if (!renv_tests_running() && file.exists("~/.Rprofile")) {
    profdeps <- dependencies("~/.Rprofile", quiet = TRUE, dev = TRUE)
    if (length(deps))
      deps <- bind_list(list(deps, profdeps))
  }

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

renv_hydrate_link_packages <- function(packages, library) {
  vprintf("* Copying packages into the cache ... ")
  cache <- renv_progress(renv_hydrate_link_package, length(packages))
  cached <- enumerate(packages, cache, library = library)
  vwritef("Done!")
  cached
}

# takes a package called 'package' installed at location 'location',
# and copies it to the library 'library'
renv_hydrate_copy_package <- function(package, location, library) {
  target <- file.path(library, package)
  renv_file_copy(location, target, overwrite = TRUE)
}

renv_hydrate_copy_packages <- function(packages, library) {
  vprintf("* Copying packages into the library ... ")
  copy <- renv_progress(renv_hydrate_copy_package, length(packages))
  copied <- enumerate(packages, copy, library = library)
  vwritef("Done!")
  copied
}

renv_hydrate_resolve_missing <- function(project, na) {

  # resolve library paths
  library <- renv_paths_library(project = project)
  libpaths <- renv_libpaths_resolve(library)
  renv_scope_libpaths(libpaths)

  # figure out which packages are missing (if any)
  packages <- names(na)
  installed <- renv_installed_packages(lib.loc = libpaths)
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

  records <- renv_retrieve(packages)
  renv_install(records)

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
