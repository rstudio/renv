
#' Remove Packages
#'
#' Remove packages currently installed within a project's private library.
#'
#' @inheritParams renv-params
#'
#' @param packages A character vector of R packages to remove.
#'
#' @export
remove <- function(packages) {
  paths <- renv_paths_library(project = renv_project(), packages)
  recursive <- renv_file_type(paths) == "directory"
  unlink(paths, recursive = recursive)
  messagef("* Removed %i %s.", length(paths), plural("package", length(paths)))
}
