
renv_cellar_roots <- function(project = NULL) {
  c(
    renv_paths_renv("cellar", project = project),
    renv_paths_renv("local", project = project),
    renv_paths_cellar(),
    renv_paths_local()
  )
}

renv_cellar_database <- function(project = NULL) {

  # find cellar root directories
  project <- renv_project_resolve(project)
  roots <- renv_cellar_roots(project)

  # list files both at top-level + one nested level
  paths <- list.files(roots, full.names = TRUE)
  paths <- c(paths, list.files(paths, full.names = TRUE))

  # keep the files that look like packages
  parsed <- renv_package_filename_parse(paths)

  data_frame(
    Package = parsed$Package,
    Version = parsed$Version,
    Path    = parsed$Path
  )

}
