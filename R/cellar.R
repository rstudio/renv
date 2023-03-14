
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

  # grab files that look like packages
  extpat <- "(?:\\.tar\\.gz|\\.tgz|\\.zip)$"
  paths <- grep(extpat, paths, value = TRUE)

  # parse into data.frame
  base <- basename(paths)
  parts <- strsplit(base, "_", fixed = TRUE)
  package <- map_chr(parts, `[[`, 1L)
  rest <- map_chr(parts, `[[`, 2L)
  version <- sub(extpat, "", rest)

  data_frame(
    Package = package,
    Version = version,
    Path    = paths
  )

}

renv_cellar_latest <- function(package, project) {

  db <- renv_cellar_database(project = project)
  db <- db[db$Package == package, ]
  db <- db[order(package_version(db$Version), decreasing = TRUE), ]
  if (nrow(db) == 0L)
    return(record)

  entry <- db[1, ]
  list(
    Package = entry$Package,
    Version = entry$Version,
    Source  = "Cellar"
  )

}
