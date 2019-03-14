
# tools for debugging -- enable to turn off 'renv's error handling,
# so such errors can reach the top level and be more easily managed
`_renv_debugging` <- FALSE

renv_debugging <- function(value) {
  if (missing(value))
    `_renv_debugging`
  else
    `_renv_debugging` <<- value
}
