
#' Clean a Project
#'
#' Clean up a project and its associated \R libraries.
#'
#' The following actions will be performed:
#'
#' - Leftover temporary directories in the project library will be removed.
#' - Non-system packages installed in the system library will be removed.
#' - Unused packages within the project will be removed.
#' - Stale lockfiles (`00LOCK-`) will be removed.
#' - Packages within the cache that are no longer used will be removed.
#'
#' @inheritParams renv-params
#'
#' @export
clean <- function(project = NULL, confirm = interactive()) {
  renv_scope_error_handler()
  project <- project %||% renv_project()

  status <-
    renv_clean_stale_lockfiles(project, confirm)  |
    renv_clean_library_tempdirs(project, confirm) |
    renv_clean_system_library(project, confirm)   |
    renv_clean_unused_packages(project, confirm)  |
    renv_clean_cache(project, confirm)

  if (status)
    vwritef("* The project has been cleaned.")
}

renv_clean_library_tempdirs <- function(project, confirm) {

  library <- renv_paths_library(project = project)
  children <- list.files(library, full.names = TRUE)

  bad <- grep("/file\\w{12}$", children, value = TRUE)
  if (empty(bad))
    return(TRUE)

  # nocov start
  if (confirm || renv_verbose()) {

    renv_pretty_print(
      bad,
      "The following directories will be removed:",
      wrap = FALSE
    )

    if (confirm && !proceed())
      return(FALSE)

  }
  # nocov end

  unlink(bad, recursive = TRUE)
  TRUE

}


# remove user packages in system library
renv_clean_system_library <- function(project, confirm) {

  db <- renv_installed_packages(lib.loc = .Library, priority = "NA")
  packages <- db$Package
  if (empty(packages))
    return(TRUE)

  # nocov start
  if (confirm || renv_verbose()) {

    renv_pretty_print(
      packages,
      "The following non-system packages are installed in the system library:",
      c(
        "These packages will be removed.",
        "Consider re-installing these packages in your site library."
      )
    )

    if (confirm && !proceed())
      return(FALSE)

  }
  # nocov end

  remove(packages, library = .Library)
  TRUE

}

renv_clean_unused_packages <- function(project, confirm) {

  # find packages used in a project and their dependencies
  deps <- dependencies(project)
  paths <- renv_dependencies(project, deps$Package)
  packages <- names(paths)

  # find packages installed in the project library
  library <- renv_paths_library(project = project)
  installed <- list.files(library)

  # figure out which packages aren't needed
  removable <- setdiff(installed, packages)
  if (empty(removable))
    return(TRUE)

  # nocov start
  if (confirm || renv_verbose()) {

    renv_pretty_print(
      removable,
      c(
        "The following packages are installed in the project library,",
        "but appear to be no longer used in your project."
      ),
      "These packages will be removed."
    )

    if (confirm && !proceed())
      return(FALSE)

  }
  # nocov end

  remove(removable, library = library)
  return(TRUE)

}

renv_clean_stale_lockfiles <- function(project, confirm) {

  # find 00LOCK directories in library
  library <- renv_paths_library(project = project)
  lock <- list.files(path = library, pattern = "^00LOCK", full.names = TRUE)
  if (empty(lock))
    return(TRUE)

  # check to see which are old
  now <- Sys.time()
  mtime <- file.mtime(lock)
  mtime[is.na(mtime)] <- now
  diff <- difftime(now, mtime, units = "mins")
  old <- lock[diff > 2]
  if (empty(old))
    return(TRUE)

  # nocov start
  if (confirm || renv_verbose()) {

    renv_pretty_print(
      basename(old),
      "The following stale lockfiles were discovered in your library:",
      "These lockfiles will be removed.",
      wrap = FALSE
    )

    if (confirm && !proceed())
      return(FALSE)

  }
  # nocov end

  unlink(old, recursive = TRUE)
  TRUE
}

renv_clean_cache <- function(project, confirm) {

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

    if (confirm && !proceed()) {
      message("* Operation aborted.")
      return(FALSE)
    }

    writeLines(projlist[!missing], projects, useBytes = TRUE)

  }

  action <- function(project) {
    library <- renv_paths_library(project = project)
    packages <- list.files(library, full.names = TRUE)
    map_chr(packages, function(package) {
      record <- renv_description_read(package)
      record$Hash <- renv_hash_description(package)
      renv_cache_package_path(record)
    }, USE.NAMES = FALSE)
  }

  # for each project, find packages used in their renv private library,
  # and look for entries in the cache
  projlist <- projlist[!missing]
  vprintf("* Enumerating packages used by renv projects ... ")
  used <- uapply(projlist, renv_progress(action, length(projlist)))
  vwritef("Done!")

  # check what packages are actually available in the cache
  available <- renv_cache_list()

  diff <- setdiff(available, used)
  if (empty(diff)) {
    vwritef("* The cache is up to date.")
    return(TRUE)
  }

  # nocov start
  if (confirm || renv_verbose()) {

    renv_pretty_print(
      renv_cache_format_path(diff),
      "The following packages are installed in the cache but no longer used:",
      "These packages will be removed.",
      wrap = FALSE
    )

    if (confirm && !proceed())
      return(FALSE)

  }
  # nocov end

  # remove the directories
  unlink(diff, recursive = TRUE)

  # remove parent directories if they're empty
  parents <- dirname(diff)
  for (i in 1:3) {

    keep <- map_lgl(parents, function(x) {
      files <- list.files(x, all.files = TRUE, no.. = TRUE)
      length(files) == 0
    }, USE.NAMES = FALSE)

    parents <- parents[keep]
    if (empty(parents))
      break

    unlink(parents, recursive = TRUE)
    parents <- dirname(parents)

  }

  vwritef("* %i package(s) have been removed.", length(diff))
  return(TRUE)

}
