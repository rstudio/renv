
#' Remove Packages
#'
#' Remove (uninstall) \R packages.
#'
#' @inheritParams renv-params
#'
#' @param packages A character vector of \R packages to remove.
#' @param library The library from which packages should be removed. When
#'   `NULL`, the active library (that is, the first entry reported in
#'   `.libPaths()`) is used instead.
#'
#' @export
remove <- function(packages, library = NULL) {

  library <- library %||% renv_libpaths_default()

  if (library == renv_paths_library(project = project)) {
    vwritef("* Removing package(s) from project library ...")
  } else {
    fmt <- "* Removing package(s) from library '%s' ..."
    vwritef(fmt, aliased_path(library))
  }

  if (length(packages) == 1) {
    count <- as.numeric(renv_remove_package(packages, library))
    return(invisible(count))
  }

  count <- 0
  for (package in packages) {
    if (renv_remove_package(package, library))
      count <- count + 1
  }

  vwritef("* Done! Removed %i %s.", count, plural("package", count))
  invisible(count)

}

renv_remove_package <- function(package, library) {

  path <- file.path(library, package)
  if (!renv_file_exists(path)) {
    vwritef("* Package '%s' is not installed -- nothing to do.", package)
    return(FALSE)
  }

  recursive <- renv_file_type(path) == "directory"
  vprintf("Removing package '%s' ... ", package)
  unlink(path, recursive = recursive)
  vwritef("Done!")

  TRUE

}
