
#' Hydrate a Project
#'
#' Discover the \R packages used within a project, and then install those
#' packages into the active library.
#'
#' While this function is normally called as part of [init()], it may be useful
#' to call this function explicitly when working with a new project, as it
#' can take care of finding and installing \R packages available on CRAN that
#' have not yet been installed on your machine.
#'
#' @inheritParams renv-params
#'
#' @param library The \R library to be hydrated. When `NULL`, the active
#'   library as reported by `.libPaths()` is used.
#'
#' @export
hydrate <- function(project = NULL, library = NULL) {
  project <- project %||% renv_project()
  library <- library %||% renv_libpaths_default()

  # find packages used in this project, and the dependencies of those packages
  deps <- renv_hydrate_dependencies(project)

  # remove 'renv' since it's managed separately
  deps$renv <- NULL

  # remove base + missing packages
  base <- renv_installed_packages_base()
  na <- deps[is.na(deps)]
  packages <- deps[setdiff(names(deps), c(names(na), rownames(base)))]

  # get and construct path to library
  ensure_directory(library)

  # copy packages from user library to cache
  cacheable <-
    library == renv_paths_library(project = project) &&
    settings$use.cache(project = project)

  if (cacheable)
    renv_hydrate_cache_packages(packages, library)
  else
    renv_hydrate_copy_packages(packages, library)

  # update the library paths so that we're using the newly-established library
  renv_libpaths_set(library)

  # attempt to install missing packages (if any)
  renv_hydrate_resolve_missing(na)

  # we're done!
  invisible(packages)

}

renv_hydrate_dependencies <- function(project) {
  vprintf("* Discovering package dependencies ... ")
  deps <- dependencies(project)
  ignored <- c("renv", settings$ignored.packages(project = project))
  packages <- setdiff(unique(deps$Package), ignored)
  all <- renv_dependencies(project, packages)
  vwritef("Done!")
  all
}

# takes a package called 'package' installed at location 'location',
# copies that package into the cache, and then links from the cache
# to the (private) library 'library'
renv_hydrate_cache_package <- function(package, location, library) {

  # construct path to cache
  record <- renv_snapshot_description(location)
  cache <- renv_cache_package_path(record)

  # copy package into the cache
  if (!renv_file_exists(cache)) {
    ensure_parent_directory(cache)
    renv_file_copy(location, cache)
  }

  # link package back from cache to library
  # (only for private libraries)
  target <- file.path(library, package)
  ensure_parent_directory(target)
  if (path_within(target, renv_paths_library()))
    renv_file_link(cache, target, overwrite = TRUE)

}

renv_hydrate_cache_packages <- function(packages, library) {
  vprintf("* Copying packages into the cache ... ", appendLF = FALSE)
  cache <- renv_progress(renv_hydrate_cache_package, length(packages))
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

renv_hydrate_resolve_missing <- function(na) {

  packages <- names(na)
  installed <- renv_installed_packages(lib.loc = renv_paths_library())
  if (all(packages %in% installed$Package))
    return()

  vwritef("* Resolving missing dependencies  ... ")

  renv_restore_begin(packages = packages)
  on.exit(renv_restore_end(), add = TRUE)
  records <- renv_restore_retrieve(packages)
  records <- Filter(renv_install_required, records)
  renv_restore_install(records)

}
