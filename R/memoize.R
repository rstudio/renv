
the$memoize <- new.env(parent = emptyenv())

memo <- function(value, scope = NULL) {
  scope <- scope %||% stringify(sys.call(sys.parent())[[1L]])
  (the$memoize[[scope]] <- the$memoize[[scope]] %||% value)
}

memoize <- function(key, value, scope = NULL) {

  # figure out scope to use
  scope <- scope %||% stringify(sys.call(sys.parent())[[1L]])

  # initialize memoized environment
  envir <-
    the$memoize[[scope]] <-
    the$memoize[[scope]] %||%
    new.env(parent = emptyenv())

  # retrieve, or compute, memoized value
  envir[[key]] <- envir[[key]] %||% value

}
