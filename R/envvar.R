

renv_envvar_get <- function(key, unset = NA) {
  Sys.getenv(key, unset = unset)
}

renv_envvar_set <- function(key, value) {

  # handle NULL / NA values
  if (is.null(value) || is.na(value))
    return(Sys.unsetenv(key))

  # otherwise, delegate
  args <- structure(list(value), names = key)
  do.call(Sys.setenv, args)

}

renv_envvar_clear <- function(key) {
  Sys.unsetenv(key)
}

renv_envvar_path_add <- function(envvar, value, prepend = TRUE) {

  old <- Sys.getenv(envvar, unset = "")
  old <- strsplit(old, .Platform$path.sep)[[1]]

  parts <- if (prepend) union(value, old) else union(old, value)
  new <- paste(parts, collapse = .Platform$path.sep)

  names(new) <- envvar
  do.call(Sys.setenv, as.list(new))

  new

}

renv_envir_exists <- function(key) {
  !is.na(Sys.getenv(key, unset = NA))
}
