
#' Hydrate a Virtual Environment
#'
#' Discover package dependencies within a project, and then ensure those
#' dependencies are all installed in the requested virtual environment.
#'
#' @inheritParams renv-params
#'
#' @export
hydrate <- function(name = NULL, project = NULL) {

  # make sure we have an active virtual environment
  name <- name %||% renv_state$environment()
  ensure_existing_renv(name)

  # find packages used in this project, and the dependencies of those packages
  project <- project %||% renv_state$project()
  deps <- renv_hydrate_discover_dependencies(project)

  # remove 'renv' since it's managed separately
  deps$renv <- NULL

  # remove base + missing packages
  base <- installed.packages(lib.loc = .Library, priority = "base")
  na <- deps[is.na(deps)]
  packages <- deps[setdiff(names(deps), c(names(na), rownames(base)))]

  # copy packages from user library to cache
  library <- renv_paths_library(name)
  ensure_directory(library)
  renv_hydrate_cache_packages(packages, library, name)

  # update the library paths so that we're using the newly-established library
  renv_libpaths_set(library)

  # attempt to install missing packages (if any)
  renv_hydrate_resolve_missing(na)

  # we're done!
  invisible(packages)

}

renv_hydrate_discover_dependencies <- function(project) {
  vmessagef("* Discovering package dependencies ... ", appendLF = FALSE)
  deps <- discover_dependencies(project)
  all <- renv_dependencies(unique(deps$Package))
  vmessagef("Done!")
  all
}

renv_hydrate_cache_package <- function(package, location, name) {

  record <- renv_snapshot_description(location, name)
  cache <- renv_cache_package_path(record)
  if (file.exists(cache))
    return(cache)

  if (!file.exists(cache)) {
    ensure_parent_directory(cache)
    renv_file_copy(location, cache)
  }

  cache

}

renv_hydrate_cache_packages <- function(packages, library, name) {
  vmessagef("* Copying packages into the cache  ... ", appendLF = FALSE)
  cached <- enumerate(packages, renv_hydrate_cache_package, name = name)
  enumerate(cached, function(package, cache) {
    target <- file.path(library, package)
    ensure_parent_directory(target)
    renv_file_link(cache, target, overwrite = TRUE)
  })
  vmessagef("Done!")
  cached
}

renv_hydrate_resolve_missing <- function(na) {

  if (!length(na))
    return()

  vmessagef("* Resolving missing dependencies  ... ")

  # TODO: if we have a manifest, should we use it?
  renv_restore_begin()
  on.exit(renv_restore_end(), add = TRUE)

  packages <- names(na)
  status <- lapply(packages, function(package) {
    catch(renv_restore_install(package))
  })

  renv_hydrate_report_restore_failure(packages, status)

}

renv_hydrate_report_restore_failure <- function(packages, status) {

  bad <- map_lgl(status, inherits, "error")
  if (!any(bad)) {
    vmessagef("* Missing dependencies successfully resolved.")
    return(status)
  }

  msg <- lines(
    "The following inferred package(s) could not be resolved:",
    "",
    paste("-", paste(shQuote(packages[bad]), collapse = ", ")),
    "",
    "Please install these packages manually, or ignore them in your project."
  )
  vmessagef(msg)

  status

}
