
renv_scope_libpaths <- function(new) {
  old <- .libPaths(); .libPaths(new)
  defer(.libPaths(old), envir = parent.frame())
}
