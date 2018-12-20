
is_scalar <- function(object, class) {
  length(object) == 1 && inherits(object, class)
}

is_string <- function(object) {
  is.character(object) && length(object) == 1
}
