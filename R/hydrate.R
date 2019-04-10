
#' Hydrate a Project
#'
#' Discover the \R packages used within a project, and then install those
#' packages into the active library. This effectively allows you to clone the
#' state of your system R libraries for use within a project library.
#'
#' `hydrate()` attempts to re-use packages already installed on your system, to
#' avoid unnecessary attempts to download and install packages from CRAN.
#' `hydrate()` will attempt to discover \R packages from the following sources
#' (in order):
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
#' The `missing` argument controls what happens when your project requests the
#' hydration of packages which are not currently installed on your system. The
#' possible values are:
#'
#' \tabular{ll}{
#' **Option**   \tab **Action** \cr
#' `"install"`  \tab Install the latest version of missing packages from CRAN. \cr
#' `"ignore"`   \tab Do nothing; missing packages will not be installed. \cr
#' }
#'
#' The \R option `renv.hydrate.missing` can be used to control the default behavior
#' of `renv::hydrate()`.
#'
#' @inheritParams renv-params
#'
#' @param packages The set of \R packages to install. When `NULL`, the
#'   set of packages as reported by [dependencies()] is used.
#'
#' @param library The \R library to be hydrated. When `NULL`, the active
#'   library as reported by `.libPaths()` is used.
#'
#' @param missing The behavior to choose when one or more of the requested
#'   packages are not available on the system. See **Missing Packages**
#'   for more details.
#'
#' @export
hydrate <- function(packages = NULL,
                    project = NULL,
                    library = NULL,
                    missing = NULL)
{
  project  <- project  %||% renv_project()
  library  <- library  %||% renv_libpaths_default()

  missing <- missing %||% getOption("renv.hydrate.missing", default = "install")
  missing <- match.arg(missing, c("install", "ignore"))

  # find packages used in this project, and the dependencies of those packages
  deps <- renv_hydrate_dependencies(project, packages)

  # remove 'renv' since it's managed separately
  deps$renv <- NULL

  # remove base + missing packages
  base <- renv_installed_packages_base()
  na <- deps[is.na(deps)]
  packages <- deps[setdiff(names(deps), c(names(na), rownames(base)))]

  # get and construct path to library
  ensure_directory(library)

  # copy packages from user library to cache
  linkable <-
    library == renv_paths_library(project = project) &&
    settings$use.cache(project = project)

  if (linkable)
    renv_hydrate_link_packages(packages, library)
  else
    renv_hydrate_copy_packages(packages, library)

  # update the library paths so that we're using the newly-established library
  renv_libpaths_set(library)

  # attempt to install missing packages (if any)
  if (missing == "install")
    renv_hydrate_resolve_missing(project, na)

  # we're done!
  invisible(packages)

}

renv_hydrate_dependencies <- function(project, packages = NULL) {
  vprintf("* Discovering package dependencies ... ")
  packages <- packages %||% unique(dependencies(project)$Package)
  ignored <- c("renv", settings$ignored.packages(project = project))
  packages <- setdiff(packages, ignored)
  all <- renv_dependencies(project, packages)
  vwritef("Done!")
  all
}

# takes a package called 'package' installed at location 'location',
# copies that package into the cache, and then links from the cache
# to the (private) library 'library'
renv_hydrate_link_package <- function(package, location, library) {

  # construct path to cache
  record <- renv_snapshot_description(location)
  cache <- renv_cache_package_path(record)

  # copy package into the cache
  if (!renv_file_exists(cache)) {
    ensure_parent_directory(cache)
    renv_file_copy(location, cache)
  }

  # link package back from cache to library
  target <- file.path(library, package)
  ensure_parent_directory(target)
  renv_file_link(cache, target, overwrite = TRUE)

}

renv_hydrate_link_packages <- function(packages, library) {
  vprintf("* Copying packages into the cache ... ", appendLF = FALSE)
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
  vprintf("* Copying packages into the library ... ", appendLF = FALSE)
  copy <- renv_progress(renv_hydrate_copy_package, length(packages))
  copied <- enumerate(packages, copy, library = library)
  vwritef("Done!")
  copied
}

renv_hydrate_resolve_missing <- function(project, na) {

  packages <- names(na)
  library <- renv_paths_library(project = project)
  installed <- renv_installed_packages(lib.loc = library)
  if (all(packages %in% installed$Package))
    return()

  vwritef("* Resolving missing dependencies  ... ")

  renv_restore_begin(packages = packages)
  on.exit(renv_restore_end(), add = TRUE)
  records <- renv_retrieve(packages)
  renv_install(project, records)

}
