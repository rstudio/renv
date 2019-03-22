
renv_scope_libpaths <- function(new) {
  old <- renv_libpaths_set(new)
  defer(renv_libpaths_set(old), envir = parent.frame())
}

renv_scope_options <- function(...) {

  new <- list(...)
  old <- lapply(names(new), getOption)
  names(old) <- names(new)

  do.call(base::options, new)
  defer(do.call(base::options, old), envir = parent.frame())

}

renv_scope_locale <- function(category = "LC_ALL", locale = "") {
  saved <- Sys.getlocale(category)
  defer(Sys.setlocale(category, saved), envir = parent.frame())
}
