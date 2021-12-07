
recurse <- function(object, callback, ...) {
  renv_recurse_impl(list(), object, callback, ...)
}

renv_recurse_impl <- function(stack, object, callback, ...) {

  # ignore missing values
  if (missing(object) || identical(object, quote(expr = )))
    return(FALSE)

  # push node on to stack
  stack[[length(stack) + 1]] <- object

  # invoke callback
  result <- callback(object, stack, ...)
  if (is.call(result))
    object <- result
  else if (identical(result, FALSE))
    return(FALSE)

  # recurse
  if (is.recursive(object))
    for (i in seq_along(object))
      renv_recurse_impl(stack, object[[i]], callback, ...)

}
