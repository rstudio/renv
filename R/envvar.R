
renv_envvar_path_add <- function(envvar, value, prepend = TRUE) {

  old <- Sys.getenv(envvar, unset = "")
  old <- strsplit(old, .Platform$path.sep)[[1]]

  parts <- if (prepend) union(value, old) else union(old, value)
  new <- paste(parts, collapse = .Platform$path.sep)

  names(new) <- envvar
  do.call(Sys.setenv, as.list(new))

  new

}

renv_envvar_exists <- function(key) {
  !is.na(Sys.getenv(key, unset = NA))
}
