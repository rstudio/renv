
renv_libpaths_default <- function() {
  .libPaths()[1]
}

renv_libpaths_all <- function() {
  .libPaths()
}

renv_libpaths_set <- function(libpaths) {
  .libPaths(libpaths)
}
