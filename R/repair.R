
# NOTE: mostly for internal use when i accidentally
# link packages in the user library into the cache
renv_repair <- function(library = NULL) {
  library <- library %||% renv_libpaths_default()
  paths <- list.files(library, full.names = TRUE)
  links <- Sys.readlink(paths)
  broken <- nzchar(links) & !file.exists(links)
  packages <- basename(paths[broken])
  install(packages, library = library)
}
