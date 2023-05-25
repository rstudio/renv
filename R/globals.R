
`_renv_globals` <- new.env(parent = emptyenv())

global <- function(name, value) {
  `_renv_globals`[[name]] <- `_renv_globals`[[name]] %||% value
}
