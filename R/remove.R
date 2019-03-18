
#' Remove Packages
#'
#' Remove packages currently installed within a project's private library.
#'
#' @inheritParams renv-params
#'
#' @param packages A character vector of R packages to remove.
#' @param library The library from which packages should be removed. When
#'   `NULL`, the active library is used instead.
#'
#' @export
remove <- function(packages, library = NULL) {
  library <- library %||% renv_libpaths_default()
  paths <- file.path(library, packages)
  recursive <- renv_file_type(paths) == "directory"
  unlink(paths, recursive = recursive)
  messagef("* Removed %i %s.", length(paths), plural("package", length(paths)))
}
