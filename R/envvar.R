

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

renv_envvar_modify <- function(envvar, value, prepend) {

  old <- Sys.getenv(envvar, unset = NA)

  parts <- if (prepend)
    c(value, if (!is.na(old)) old)
  else
    c(if (!is.na(old)) old, value)

  new <- paste(unique(parts), collapse = .Platform$path.sep)

  names(new) <- envvar
  do.call(Sys.setenv, as.list(new))

  new

}

renv_envvar_prepend <- function(envvar, value) {
  renv_envvar_modify(envvar, value, TRUE)
}

renv_envvar_append <- function(envvar, value) {
  renv_envvar_modify(envvar, value, FALSE)
}
