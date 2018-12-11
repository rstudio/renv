
`_renv_globals` <- new.env(parent = emptyenv())

renv_global_get <- function(name) {
  get(name, envir = `_renv_globals`, inherits = FALSE)
}

renv_global_set <- function(name, value) {
  assign(name, value, envir = `_renv_globals`, inherits = FALSE)
}

renv_global_clear <- function(name) {
  rm(list = name, envir = `_renv_globals`)
}

