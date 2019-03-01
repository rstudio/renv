
#' Clean a Project
#'
#' Clean up a project. The following actions will be performed:
#'
#' - Leftover temporary directories in the project library will be removed.
#'
#' @inheritParams renv-params
#'
#' @export
clean <- function(project = NULL, confirm = interactive()) {
  project <- project %||% renv_project()
  renv_clean_library_tempdirs(project, confirm)
}

renv_clean_library_tempdirs <- function(project, confirm) {

  library <- renv_paths_library(project)
  children <- list.files(library, full.names = TRUE)

  bad <- grep("/file\\w{12}$", children, value = TRUE)
  if (empty(bad))
    return(character())

  if (confirm || renv_verbose()) {

    renv_pretty_print_packages(
      bad,
      "The following directories will be removed:",
    )
    writeLines("")

    if (!proceed()) {
      writeLines("Operation aborted.")
      return(character())
    }

  }

  unlink(bad, recursive = TRUE)

}


# remove user packages in system library
renv_clean_system_library <- function() {
  packages <- installed.packages(lib.loc = .Library, priority = "NA")
  remove.packages(rownames(packages), lib = .Library)
}
