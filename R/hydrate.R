
#' Copy packages from user libraries to a project library
#'
#' @description
#' `hydrate()` installs missing packages from a user library into the project
#' library. `hydrate()` is called automatically by [init()], and it is rare
#' that you should need it otherwise, as it can easily get your project into
#' an inconsistent state.
#'
#' It may very occasionally be useful to call `hydrate(update = "all")` if you
#' want to update project packages to match those installed in your global
#' library (as opposed to using [update()] which will get the latest versions
#' from CRAN). In this case, you should verify that your code continues to work,
#' then call [snapshot()] to record updated package versions in the lockfile.
#'
#' @inherit renv-params
#'
#' @param packages The set of \R packages to install. When `NULL`, the
#'   packages found by [dependencies()] are used.
#'
#' @param library The \R library to be hydrated. When `NULL`, the active
#'   library as reported by `.libPaths()` is used.
#'
#' @param repos The \R repositories to be used. If the project depends on any
#'   \R packages which cannot be found within the user library paths, then
#'   those packages will be installed from these repositories instead.
#'
#' @param update Boolean; should `hydrate()` attempt to update already-installed
#'   packages if the requested package is already installed in the project
#'   library? Set this to `"all"` if you'd like _all_ packages to be refreshed
#'   from the source library if possible.
#'
#' @param sources A vector of library paths where renv should look for packages.
#'   When `NULL` (the default), `hydrate()` will look in the system libraries
#'   (the user library, the site library and the default library) then the
#'   renv cache.
#'
#'   If a package is not found in any of these locations, `hydrate()`
#'   will try to install it from the active R repositories.
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
#' @keywords internal
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
                    repos   = getOption("repos"),
                    update  = FALSE,
                    sources = NULL,
                    prompt  = interactive(),
                    report  = TRUE,
                    project = NULL)
{
  renv_scope_error_handler()
  renv_dots_check(...)

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)
  renv_scope_verbose_if(prompt)

  renv_activate_prompt("hydrate", library, prompt, project)

  renv_scope_options(repos = repos)
  library <- renv_path_normalize(library %||% renv_libpaths_active())
  packages <- packages %||% renv_hydrate_packages(project)

  # find packages used in this project, and the dependencies of those packages
  deps <- renv_hydrate_dependencies(project, packages, sources)

  # remove 'renv' since it's managed separately
  deps$renv <- NULL

  # figure out required packages which aren't installed
  missing <- deps[!nzchar(deps)]

  # also consider remotes; if a package is listed within Remotes,
  # then choose to install that package instead of linking it
  filter <- function(specs, remotes) {
    packages <- map_chr(remotes, `[[`, "Package")
    keep(specs, packages)
  }

  remotes <- renv_project_remotes(project, filter = filter, resolve = TRUE)
  missing[map_chr(remotes, `[[`, "Package")] <- ""

  # remove base + missing packages
  base <- renv_packages_base()
  packages <- deps[renv_vector_diff(names(deps), c(names(missing), base))]

  # figure out if we will copy or link
  linkable <- renv_cache_linkable(project = project, library = library)

  # get and construct path to library
  ensure_directory(library)

  # only hydrate with packages that are either not currently installed,
  # or (if update = TRUE) the version in the library is newer
  packages <- renv_hydrate_filter(packages, library, update)

  # inform user about changes
  if (report) {
    renv_hydrate_report(packages, missing, linkable)
    if (length(packages) || length(missing))
      cancel_if(prompt && !proceed())
  }

  # check for nothing to be done
  if (empty(packages) && empty(missing)) {
    if (report)
      writef("- No new packages were discovered in this project; nothing to do.")
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
    fmt <- "- Hydrated %s packages in %s."
    writef(fmt, length(packages), renv_difftime_format(time))
  }

  # attempt to install missing packages (if any)
  missing <- renv_hydrate_resolve_missing(project, library, remotes, missing)

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
  if (file.exists(descpath)) {
    desc <- catch(renv_description_read(path = descpath))
    if (inherits(desc, "error"))
      return(TRUE)
  }

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

renv_hydrate_packages <- function(project) {
  renv_snapshot_dependencies(project, dev = TRUE)
}

renv_hydrate_dependencies <- function(project,
                                      packages = NULL,
                                      libpaths = NULL)
{
  ignored <- renv_project_ignored_packages(project = project)
  packages <- renv_vector_diff(packages, ignored)
  libpaths <- libpaths %||% renv_hydrate_libpaths()
  renv_package_dependencies(packages, libpaths = libpaths, project = project)
}

# NOTE: we don't want to look in user / site libraries when testing
# on CRAN, as we may accidentally find versions of packages available
# on CRAN but not that we want to use during tests
renv_hydrate_libpaths <- function() {

  conf <- config$hydrate.libpaths()
  if (is.character(conf) && length(conf))
    conf <- unlist(strsplit(conf, ":", fixed = TRUE))

  libpaths <- case(
    renv_tests_running() ~ character(),
    length(conf) ~ conf,
    ~ c(
      renv_libpaths_default(),
      renv_libpaths_user(),
      renv_libpaths_site(),
      renv_libpaths_system()
    )
  )

  libpaths <- .expand_R_libs_env_var(libpaths)
  unique(renv_path_normalize(libpaths))

}

# takes a package called 'package' installed at location 'location',
# copies that package into the cache, and then links from the cache
# to the (private) library 'library'
renv_hydrate_link_package <- function(package, location, library) {

  # construct path to cache
  record <- catch(renv_snapshot_description(location))
  if (inherits(record, "error"))
    return(FALSE)

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

  if (renv_path_same(library, renv_paths_library(project = project)))
    printf("- Linking packages into the project library ... ")
  else
    printf("- Linking packages into %s ... ", renv_path_pretty(library))

  callback <- renv_progress_callback(renv_hydrate_link_package, length(packages))
  cached <- enumerate(packages, callback, library = library)
  writef("Done!")
  cached

}

# takes a package called 'package' installed at location 'location',
# and copies it to the library 'library'
renv_hydrate_copy_package <- function(package, location, library) {
  target <- file.path(library, package)
  renv_file_copy(location, target, overwrite = TRUE)
}

renv_hydrate_copy_packages <- function(packages, library, project) {

  if (renv_path_same(library, renv_paths_library(project = project)))
    printf("- Copying packages into the project library ... ")
  else
    printf("- Copying packages into %s ... ", renv_path_pretty(library))

  callback <- renv_progress_callback(renv_hydrate_copy_package, length(packages))
  copied <- enumerate(packages, callback, library = library)
  writef("Done!")
  copied
}

renv_hydrate_resolve_missing <- function(project, library, remotes, missing) {

  # make sure requested library is made active
  #
  # note that we only want to place the requested library on the library path;
  # we want to ensure that all required packages are hydrated into the
  # requested library
  #
  # https://github.com/rstudio/renv/issues/1177
  ensure_directory(library)
  renv_scope_libpaths(library)

  packages <- names(missing)
  if (empty(packages))
    return()

  writef("- Resolving missing dependencies ... ")

  # define a custom error handler for packages which we cannot retrieve
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
    records  = remotes,
    handler  = handler
  )

  records <- renv_retrieve_impl(packages)
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

    caution_bullets(
      "The following package(s) were not installed successfully:",
      text,
      "You may need to manually download and install these packages."
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
      preamble = preamble,
      old = list(),
      new = records,
      postamble = postamble,
      formatter = formatter
    )

  }

  if (length(na)) {
    caution_bullets(
      "The following packages are used in this project, but not available locally:",
      csort(names(na)),
      "renv will attempt to download and install these packages."
    )
  }

}
