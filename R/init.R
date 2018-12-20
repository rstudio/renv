
#' Initialize a Project-local Virtual Environment
#'
#' Discover packages used within the current project, and then initialize
#' a virtual environment with those packages.
#'
#' @param project The project directory.
#' @param ... Optional arguments passed to [create()].
#'
#' @export
init <- function(project = NULL, ...) {
  project <- project %||% getwd()

  # switch to local mode
  local <- renv_local()
  renv_set_local(TRUE)
  on.exit(renv_set_local(local), add = TRUE)

  # create the virtual environment
  # TODO: what action to take if environment already exists?
  name <- basename(project)
  create(name = name, r_libs = name, ..., overwrite = TRUE)

  # find packages used in this project
  vmessagef("Discovering package dependencies ... ", appendLF = FALSE)
  deps <- discover_dependencies(project)
  vmessagef("Done!")

  # discover recursive dependencies of these packages
  # TODO: since we parse DESCRIPTION files to get these, makes these bits
  # user-configurable (e.g. should we include Suggets?)
  all <- renv_dependencies(unique(deps$Package))

  # keep only non-base packages
  base <- installed.packages(lib.loc = .Library, priority = "base")
  packages <- all[setdiff(names(all), base)]

  # copy these packages into the cache (if they aren't already cached packages)
  vmessagef("Copying packages into the cache  ... ", appendLF = FALSE)
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

  # update the library paths so that we're using the newly-established library
  # (ensure the upcoming snapshot captures its state)
  renv <- find.package("renv", quiet = TRUE)
  .libPaths(c(library, dirname(renv)))

  # now we can activate the local environment
  activate(name, project)

}
