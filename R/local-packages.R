
renv_local_packages_database <- function(project) {

  roots <- c(
    renv_paths_project("renv/local", project = project),
    renv_paths_local()
  )

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

  data.frame(
    Package = package,
    Version = version,
    Path    = paths,
    stringsAsFactors = FALSE
  )

}

renv_local_packages_latest <- function(package, project) {

  db <- renv_local_packages_database(project = project)
  db <- db[db$Package == package, ]
  db <- db[order(package_version(db$Version), decreasing = TRUE), ]
  if (nrow(db) == 0L)
    return(record)

  entry <- db[1, ]
  list(
    Package   = entry$Package,
    Version   = entry$Version,
    Source    = "Local",
    RemoteUrl = entry$Path
  )

}
