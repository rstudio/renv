
recurse <- function(object, callback, ...) {
  callback(object, ...)
  if (is.recursive(object))
    for (i in seq_along(object))
      recurse(object[[i]], callback, ...)
}
