
invoke <- function(object) {
  if (is.function(object))
    object()
  else
    object
}
