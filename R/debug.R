
# tools for debugging -- enable to turn off 'renv's error handling,
# so such errors can reach the top level and be more easily managed
`_renv_debug` <- FALSE

renv_debug <- function(value) {
  if (missing(value))
    `_renv_debug`
  else
    `_renv_debug` <<- value
}
