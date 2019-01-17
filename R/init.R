
#' Initialize a Project-local Virtual Environment
#'
#' Discover packages used within the current project, and then initialize a
#' project-local virtual environment with those packages. The
#' currently-installed versions of any packages in use (as detected within the
#' user library) are then added to the project manifest, effectively forking
#' the state of your user library into a private project library.
#'
#' The primary steps taken when initializing a new virtual environment are:
#'
#' 1. \R package dependencies are discovered within the \R files used within
#'    the project;
#'
#' 2. Discovered packages are copied into the `renv` global package cache (so
#'    these packages can be re-used across multiple projects as necessary),
#'
#' 3. Any missing \R package dependencies discovered are then installed into
#'    a private project library,
#'
#' 4. The [activate()] function is called to activate the newly-created
#'    virtual environment.
#'
#' This mimics the workflow provided by `packrat::init()`, but with more
#' reasonable default behaviors -- in particular, `renv` does not attempt
#' to download and store package sources, and `renv` will re-use packages
#' that have already been installed whenever possible.
#'
#' @param project The project directory.
#' @param ... Optional arguments passed to [create()].
#' @param force Boolean; force initialization? By default, `renv` will refuse
#'   to initialize the home directory, or parents of the home directory.
#'
#' @export
init <- function(project = NULL, ..., force = FALSE) {
  project <- project %||% getwd()

  # ensure this is a valid project directory
  renv_init_validate_project(project, force)

  # switch to local mode
  renv_state$local(TRUE)

  # create the virtual environment
  # TODO: what action to take if environment already exists?
  # - re-create the environment and re-run the dependency discovery + caching?
  # - skip those steps and just re-use the existing environment?
  name <- basename(project)
  create(name = name, r_libs = name, ..., overwrite = TRUE)

  # find packages used in this project, and the dependencies of those packages
  deps <- renv_init_discover_dependencies(project)

  # remove base + missing packages
  base <- installed.packages(lib.loc = .Library, priority = "base")
  na <- deps[is.na(deps)]
  packages <- deps[setdiff(names(deps), c(names(na), rownames(base)))]

  # copy packages from user library to cache
  library <- renv_paths_library(name)
  ensure_directory(library)
  renv_init_cache_packages(packages, library, name)

  # update the library paths so that we're using the newly-established library
  renv_libpaths_set(library)

  # attempt to install missing packages (if any)
  renv_init_resolve_missing(na)

  # now we can activate the local environment
  activate(name, project)

}

renv_init_discover_dependencies <- function(project) {
  vmessagef("* Discovering package dependencies ... ", appendLF = FALSE)
  deps <- discover_dependencies(project)
  all <- renv_dependencies(unique(deps$Package))
  vmessagef("Done!")
  all
}

renv_init_cache_package <- function(package, location, name) {

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

renv_init_cache_packages <- function(packages, library, name) {
  vmessagef("* Copying packages into the cache  ... ", appendLF = FALSE)
  cached <- enumerate(packages, renv_init_cache_package, name = name)
  enumerate(cached, function(package, cache) {
    target <- file.path(library, package)
    unlink(target, recursive = TRUE)
    ensure_parent_directory(target)
    renv_file_link(cache, target)
  })
  vmessagef("Done!")
  cached
}

renv_init_resolve_missing <- function(na) {

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

  renv_init_report_restore_failure(packages, status)

}

renv_init_validate_project <- function(project, force) {

  # allow all project directories when force = TRUE
  if (force) return(TRUE)

  # disallow attempts to initialize renv in the home directory
  home <- path.expand("~/")
  msg <- if (renv_file_same(project, home))
    "refusing to initialize project in home directory"
  else if (path_within(home, project))
    sprintf("refusing to initialize project in directory '%s'", project)

  if (!is.null(msg)) {
    msg <- paste(msg, "(use 'force = TRUE' to override)")
    stopf(msg)
  }

}

renv_init_report_restore_failure <- function(packages, status) {

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
