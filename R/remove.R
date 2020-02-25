
#' Remove Packages
#'
#' Remove (uninstall) \R packages.
#'
#' @inherit renv-params
#'
#' @param packages A character vector of \R packages to remove.
#' @param library The library from which packages should be removed. When
#'   `NULL`, the active library (that is, the first entry reported in
#'   `.libPaths()`) is used instead.
#'
#' @return A vector of package records, describing the packages (if any) which
#'   were successfully removed.
#'
#' @export
#'
#' @example examples/examples-init.R
remove <- function(packages,
                   ...,
                   library = NULL,
                   project = NULL)
{
  renv_scope_error_handler()
  renv_dots_check(...)
  project <- renv_project_resolve(project)
  library <- library %||% renv_libpaths_default()

  descpaths <- file.path(library, packages, "DESCRIPTION")
  records <- lapply(descpaths, renv_snapshot_description)
  names(records) <- packages
  records <- Filter(function(record) !inherits(record, "error"), records)

  if (library == renv_paths_library(project = project)) {
    vwritef("* Removing package(s) from project library ...")
    on.exit(renv_snapshot_auto(project = project), add = TRUE)
  } else {
    fmt <- "* Removing package(s) from library '%s' ..."
    vwritef(fmt, aliased_path(library))
  }

  if (length(packages) == 1) {
    renv_remove_impl(packages, library)
    return(invisible(records))
  }

  count <- 0
  for (package in packages) {
    if (renv_remove_impl(package, library))
      count <- count + 1
  }

  vwritef("* Done! Removed %i %s.", count, plural("package", count))
  invisible(records)
}

renv_remove_impl <- function(package, library) {

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
