
renv_scope_libpaths <- function(new) {
  old <- renv_libpaths_set(new)
  defer(renv_libpaths_set(old), envir = parent.frame())
}

renv_scope_options <- function(new) {

  old <- lapply(names(new), getOption)
  names(old) <- names(new)

  do.call(base::options, new)
  defer(do.call(base::options, old), envir = parent.frame())

}
