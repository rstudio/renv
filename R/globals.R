
`_renv_globals` <- new.env(parent = emptyenv())

renv_global_get <- function(name) {
  if (exists(name, envir = `_renv_globals`, inherits = FALSE))
    get(name, envir = `_renv_globals`, inherits = FALSE)
}

renv_global_set <- function(name, value) {
  assign(name, value, envir = `_renv_globals`, inherits = FALSE)
}

renv_global_clear <- function(name) {
  if (exists(name, envir = `_renv_globals`, inherits = FALSE))
    rm(list = name, envir = `_renv_globals`, inherits = FALSE)
}

renv_global_once <- function(name) {
  if (exists(name, envir = `_renv_globals`, inherits = FALSE))
    return(FALSE)
  assign(name, TRUE, envir = `_renv_globals`, inherits = FALSE)
  TRUE
}
