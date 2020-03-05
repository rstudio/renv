
#' Clean a Project
#'
#' Clean up a project and its associated \R libraries.
#'
#' The following actions will be taken:
#'
#' - Stale lockfiles (`00LOCK-`) will be removed.
#' - Leftover temporary directories in the project library will be removed.
#' - Non-system packages installed in the system library will be removed.
#' - Unused packages within the project will be removed.
#' - Packages within the cache that are no longer used will be removed.
#'
#' @inherit renv-params
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # clean the current project
#' renv::clean()
#'
#' }
clean <- function(project = NULL,
                  ...,
                  prompt = interactive())
{
  renv_scope_error_handler()
  renv_dots_check(...)

  project <- renv_project_resolve(project)
  renv_dependencies_scope(project, action = "clean")

  status <- any(
    renv_clean_stale_lockfiles(project, prompt),
    renv_clean_library_tempdirs(project, prompt),
    renv_clean_system_library(project, prompt),
    renv_clean_unused_packages(project, prompt)
  )

  msg <- if (status)
    "* The project has been cleaned."
  else
    "* The project is already clean."

  vwritef(msg)
  invisible(status)
}

renv_clean_library_tempdirs <- function(project, prompt) {

  ntd <- function() {
    vwritef("* No temporary directories were found in the project library.")
    FALSE
  }

  library <- renv_paths_library(project = project)
  children <- list.files(library, full.names = TRUE)

  bad <- grep("/file\\w{12}$", children, value = TRUE)
  if (empty(bad))
    return(ntd())

  # nocov start
  if (prompt || renv_verbose()) {

    renv_pretty_print(
      bad,
      "The following directories will be removed:",
      wrap = FALSE
    )

    if (prompt && !proceed())
      return(FALSE)

  }
  # nocov end

  unlink(bad, recursive = TRUE)
  TRUE

}


# remove user packages in system library
renv_clean_system_library <- function(project, prompt) {

  ntd <- function() {
    vwritef("* No non-system packages were discovered in the system library.")
    FALSE
  }

  # explicitly query for packages
  syslib <- renv_path_normalize(renv_libpaths_system(), winslash = "/", mustWork = FALSE)
  db <- renv_installed_packages(lib.loc = syslib, priority = "NA")
  packages <- db$Package

  # also look for leftover package folders
  # (primarily for Windows, where .dlls from old packages can be left behind)

  # nocov start
  if (renv_platform_windows()) {
    folders <- list.files(syslib, full.names = TRUE)
    descpaths <- file.path(folders, "DESCRIPTION")
    missing <- !file.exists(descpaths)
    packages <- union(packages, basename(folders)[missing])
  }
  # nocov end

  # check for any packages needing removal
  if (empty(packages))
    return(ntd())

  # nocov start
  if (prompt || renv_verbose()) {

    renv_pretty_print(
      packages,
      "The following non-system packages are installed in the system library:",
      c(
        "Normally, only packages distributed with R should be installed in the system library.",
        "These packages will be removed.",
        "If necessary, consider re-installing these packages in your site library."
      )
    )

    if (prompt && !proceed())
      return(FALSE)

  }
  # nocov end

  remove(packages, library = syslib)
  TRUE

}

renv_clean_unused_packages <- function(project, prompt) {

  ntd <- function() {
    vwritef("* No unused packages were found in the project library.")
    FALSE
  }

  # find packages installed in the project library
  library <- renv_paths_library(project = project)
  installed <- list.files(library)
  if (empty(installed))
    return(ntd())

  # find packages used in the project and their dependencies
  deps <- dependencies(project, progress = FALSE)
  paths <- renv_package_dependencies(deps$Package, project = project)
  packages <- names(paths)

  # figure out which packages aren't needed
  removable <- renv_vector_diff(installed, packages)
  if (empty(removable))
    return(ntd())

  # nocov start
  if (prompt || renv_verbose()) {

    renv_pretty_print(
      removable,
      c(
        "The following packages are installed in the project library,",
        "but appear to be no longer used in your project."
      ),
      "These packages will be removed."
    )

    if (prompt && !proceed())
      return(FALSE)

  }
  # nocov end

  remove(removable, library = library)
  return(TRUE)

}

renv_clean_stale_lockfiles <- function(project, prompt) {

  ntd <- function() {
    vwritef("* No stale lockfiles were found.")
    FALSE
  }

  # find 00LOCK directories in library
  library <- renv_paths_library(project = project)
  lock <- list.files(path = library, pattern = "^00LOCK", full.names = TRUE)
  if (empty(lock))
    return(ntd())

  # check to see which are old
  now <- Sys.time()
  mtime <- file.mtime(lock)
  mtime[is.na(mtime)] <- now
  diff <- difftime(now, mtime, units = "mins")
  old <- lock[diff > 2]
  if (empty(old))
    return(ntd())

  # nocov start
  if (prompt || renv_verbose()) {

    renv_pretty_print(
      basename(old),
      "The following stale lockfiles were discovered in your library:",
      "These lockfiles will be removed.",
      wrap = FALSE
    )

    if (prompt && !proceed())
      return(FALSE)

  }
  # nocov end

  unlink(old, recursive = TRUE)
  TRUE
}

# nocov start
renv_clean_cache <- function(project, prompt) {

  ntd <- function() {
    vwritef("* No unused packages were found in the renv cache.")
    FALSE
  }

  # find projects monitored by renv
  projects <- renv_paths_root("projects")
  projlist <- character()
  if (file.exists(projects))
    projlist <- readLines(projects, warn = FALSE, encoding = "UTF-8")

  # inform user if any projects are missing
  missing <- !file.exists(projlist)
  if (any(missing)) {

    renv_pretty_print(
      projlist[missing],
      "The following projects are monitored by renv, but no longer exist:",
      "These projects will be removed from renv's project list.",
      wrap = FALSE
    )

    if (prompt && !proceed())
      return(FALSE)

    writeLines(projlist[!missing], projects, useBytes = TRUE)

  }

  action <- function(project) {
    library <- renv_paths_library(project = project)
    packages <- list.files(library, full.names = TRUE)
    descs <- file.path(packages, "DESCRIPTION")
    existing <- file.exists(descs)
    map_chr(descs[existing], renv_cache_path, USE.NAMES = FALSE)
  }

  # for each project, find packages used in their renv private library,
  # and look for entries in the cache
  projlist <- projlist[!missing]
  used <- uapply(projlist, renv_progress(action, length(projlist)))

  # check what packages are actually available in the cache
  available <- renv_cache_list()

  diff <- renv_vector_diff(available, used)
  if (empty(diff))
    return(ntd())

  if (prompt || renv_verbose()) {

    renv_pretty_print(
      renv_cache_format_path(diff),
      "The following packages are installed in the cache but no longer used:",
      "These packages will be removed.",
      wrap = FALSE
    )

    if (prompt && !proceed())
      return(FALSE)

  }

  # remove the directories
  unlink(diff, recursive = TRUE)
  renv_cache_clean_empty()
  vwritef("* %i package(s) have been removed.", length(diff))
  TRUE

}
# nocov end
