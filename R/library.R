
renv_library_prune <- function(libpath) {
  children <- list.files(libpath, full.names = TRUE)
  exists <- file.exists(children)
  unlink(children[!exists])
}
