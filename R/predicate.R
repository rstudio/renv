
pscalar <- function(x) {
  length(x) == 1L
}

pstring <- function(x) {
  is.character(x) && length(x) == 1L
}

