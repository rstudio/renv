
#' Clean a Project
#'
#' Clean up a project and its associated \R libraries.
#'
#' The following actions will be performed:
#'
#' - Leftover temporary directories in the project library will be removed.
#' - Non-system packages installed in the system library will be removed.
#' - Unused packages within the project will be removed.
#'
#' @inheritParams renv-params
#'
#' @export
clean <- function(project = NULL, confirm = interactive()) {
  project <- project %||% renv_project()

  status <-
    renv_clean_library_tempdirs(project, confirm) &&
    renv_clean_system_library(project, confirm) &&
    renv_clean_unused_packages(project, confirm) &&
    renv_clean_stale_lockfiles(project, confirm)

  if (status)
    vwritef("* The project is now clean.")
}

renv_clean_library_tempdirs <- function(project, confirm) {

  library <- renv_paths_library(project)
  children <- list.files(library, full.names = TRUE)

  bad <- grep("/file\\w{12}$", children, value = TRUE)
  if (empty(bad))
    return(TRUE)

  if (confirm || renv_verbose()) {

    renv_pretty_print(
      bad,
      "The following directories will be removed:",
      wrap = FALSE
    )

    if (confirm && !proceed()) {
      writeLines("Operation aborted.")
      return(FALSE)
    }

  }

  unlink(bad, recursive = TRUE)
  TRUE

}


# remove user packages in system library
renv_clean_system_library <- function(project, confirm) {

  db <- renv_installed_packages(lib.loc = .Library, priority = "NA")
  packages <- db$Package
  if (empty(packages))
    return(TRUE)

  if (confirm || renv_verbose()) {

    renv_pretty_print(
      packages,
      "The following non-system packages are installed in the system library:",
      c(
        "These packages will be removed.",
        "Consider re-installing these packages in your site library."
      )
    )

    if (confirm && !proceed()) {
      writeLines("Operation aborted.")
      return(FALSE)
    }

  }

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

  if (confirm || renv_verbose()) {

    renv_pretty_print(
      removable,
      c(
        "The following packages are installed in the project library,",
        "but appear to be no longer used in your project."
      ),
      postamble = "These packages will be removed."
    )

    if (confirm && !proceed()) {
      writeLines("Operation aborted.")
      return(FALSE)
    }

  }

  remove(removable, library = library)
  return(TRUE)

}

renv_clean_stale_lockfiles <- function(project, confirm) {
  # TODO: find files with 00LOCK prefix in project library,
  # and detect if they appear to be 'old', and prompt to remove
}
