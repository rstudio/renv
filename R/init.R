
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
#'
#' @export
init <- function(project = NULL, ...) {
  project <- project %||% getwd()

  # switch to local mode
  renv_state$local(TRUE)

  # create the virtual environment
  # TODO: what action to take if environment already exists?
  # - re-create the environment and re-run the dependency discovery + caching?
  # - skip those steps and just re-use the existing environment?
  name <- basename(project)
  create(name = name, r_libs = name, ..., overwrite = TRUE)

  # find packages used in this project, and the dependencies of those packages
  vmessagef("* Discovering package dependencies ... ", appendLF = FALSE)
  deps <- discover_dependencies(project)
  all <- renv_dependencies(unique(deps$Package))
  vmessagef("Done!")

  # remove base + missing packages
  base <- installed.packages(lib.loc = .Library, priority = "base")
  na <- all[is.na(all)]
  packages <- all[setdiff(names(all), c(names(na), rownames(base)))]

  # copy these packages into the cache (if they aren't already cached packages)
  vmessagef("* Copying packages into the cache  ... ", appendLF = FALSE)
  cached <- enumerate(packages, function(package, location) {

    record <- renv_snapshot_description(location, name)
    cache <- renv_cache_package_path(record)
    if (file.exists(cache))
      return(cache)

    if (!file.exists(cache)) {
      ensure_parent_directory(cache)
      renv_file_copy(location, cache)
    }

    cache

  })
  vmessagef("Done!")

  # link these packages into the private library
  library <- renv_paths_library(name)
  enumerate(cached, function(package, cache) {
    target <- file.path(library, package)
    unlink(target, recursive = TRUE)
    ensure_parent_directory(target)
    renv_file_link(cache, target)
  })

  # attempt to install missing packages (if any)
  # TODO: request user confirmation? (since we're about to mutate the user lib)
  if (length(na)) local({
    vmessagef("* Resolving missing dependencies   ... ")
    # TODO: if we have a manifest, should we use it?
    renv_restore_begin(NULL)
    on.exit(renv_restore_end(), add = TRUE)
    for (package in names(na))
      renv_restore_install(package)
    vmessagef("* Missing dependencies successfully resolved.")
  })

  # update the library paths so that we're using the newly-established library
  # (ensure the upcoming snapshot captures its state)
  renv <- find.package("renv", quiet = TRUE)
  .libPaths(c(library, dirname(renv)))

  # now we can activate the local environment
  activate(name, project)

}
