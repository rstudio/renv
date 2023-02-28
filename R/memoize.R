
`_renv_memoize` <- new.env(parent = emptyenv())

memoize <- function(key, value, scope = NULL) {

  # figure out scope to use
  scope <- scope %||% as.character(sys.call(sys.parent())[[1L]])

  # initialize memoized environment
  envir <-
    `_renv_memoize`[[scope]] <-
    `_renv_memoize`[[scope]] %||%
    new.env(parent = emptyenv())


  # retrieve, or compute, memoized value
  envir[[key]] <- envir[[key]] %||% value

}

