
`_renv_globals` <- new.env(parent = emptyenv())

renv_global <- function(name, value) {
  (`_renv_globals`[[name]]) %||% (`_renv_globals`[[name]] <- value)
}

renv_global_get <- function(name) {
  `_renv_globals`[[name]]
}

renv_global_set <- function(name, value) {
  `_renv_globals`[[name]] <- value
}

renv_global_clear <- function(name) {
  if (exists(name, envir = `_renv_globals`, inherits = FALSE))
    rm(list = name, envir = `_renv_globals`, inherits = FALSE)
}

