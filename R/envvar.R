
renv_envvar_modify <- function(envvar, value, prepend) {

  old <- Sys.getenv(envvar, unset = NA)

  parts <- if (prepend)
    c(value, if (!is.na(old)) old)
  else
    c(if (!is.na(old)) old, value)

  new <- paste(parts, collapse = .Platform$path.sep)

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
