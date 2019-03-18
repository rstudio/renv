
#' Clean a Project
#'
#' Clean up a project and its associated \R libraries.
#'
#' The following actions will be performed:
#'
#' - Leftover temporary directories in the project library will be removed.
#' - Non-system packages installed in the system library will be removed.
#'
#' @inheritParams renv-params
#'
#' @export
clean <- function(project = NULL, confirm = interactive()) {
  project <- project %||% renv_project()

  status <-
    renv_clean_library_tempdirs(project, confirm) &&
    renv_clean_system_library(project, confirm)

  if (status)
    vmessagef("* The project is now clean.")
}

renv_clean_library_tempdirs <- function(project, confirm) {

  library <- renv_paths_library(project)
  children <- list.files(library, full.names = TRUE)

  bad <- grep("/file\\w{12}$", children, value = TRUE)
  if (empty(bad))
    return(TRUE)

  if (confirm || renv_verbose()) {

    renv_pretty_print_packages(
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

    renv_pretty_print_packages(
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
