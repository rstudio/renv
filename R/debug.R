
`_renv_debug` <- FALSE

renv_debug <- function(value) {
  if (missing(value))
    `_renv_debug`
  else
    `_renv_debug` <<- value
}
