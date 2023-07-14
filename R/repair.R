
#' Repair a project library
#'
#' Repair a project library whose cache symlinks have become broken.
#' renv will attempt to re-install the requisite packages.
#'
#' @inheritParams renv-params
#'
#' @param lockfile The path to a lockfile (if any). When available, renv
#'   will use the lockfile when attempting to infer the remote associated
#'   with the inaccessible version of each missing package.
#'
#' @export
repair <- function(library  = NULL,
                   lockfile = NULL,
                   project  = NULL)
{
  renv_consent_check()
  renv_scope_error_handler()

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  libpaths <- renv_path_normalize(library %||% renv_libpaths_all())
  library <- libpaths[[1L]]

  # figure out which library paths (junction points?) appear to be broken
  paths <- list.files(library, full.names = TRUE)
  broken <- renv_file_broken(paths)
  packages <- basename(paths[broken])
  if (empty(packages)) {
    fmt <- "- The project library has no broken links -- nothing to do."
    writef(fmt)
    return(invisible(packages))
  }

  # try to find records for these packages in the lockfile
  # TODO: what if one of the requested packages isn't in the lockfile?
  lockfile <- lockfile %||% renv_lockfile_load(project = project)
  records <- renv_repair_records(packages, lockfile, project)

  # install these records
  install(
    packages = records,
    library  = library,
    project  = project
  )
}

renv_repair_records <- function(packages, lockfile, project) {
  map(packages, function(package) {
    lockfile$Packages[[package]] %||% package
  })
}
