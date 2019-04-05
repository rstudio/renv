
renv_libpaths_default <- function() {
  .libPaths()[1]
}

renv_libpaths_all <- function() {
  .libPaths()
}

renv_libpaths_set <- function(libpaths) {
  oldlibpaths <- .libPaths()
  .libPaths(libpaths)
  oldlibpaths
}

# NOTE: may return more than one library path!
renv_libpaths_user <- function() {

  # if renv is active, the user library will be saved
  envvars <- c("RENV_DEFAULT_R_LIBS_USER", "R_LIBS_USER")
  for (envvar in envvars) {

    value <- Sys.getenv(envvar, unset = NA)
    if (is.na(value) || value == "<NA>" || !nzchar(value))
      next

    parts <- strsplit(value, .Platform$path.sep, fixed = TRUE)[[1]]
    return(parts)

  }

  # otherwise, default to active library (shouldn't happen but best be safe)
  renv_libpaths_default()

}

renv_libpaths_activate <- function(project) {

  libpaths <- c(
    renv_paths_library(project = project),
    settings$external.libraries(project = project)
  )

  lapply(libpaths, ensure_directory)
  oldlibpaths <- .libPaths()
  .libPaths(libpaths)
  oldlibpaths

}
