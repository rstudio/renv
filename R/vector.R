
renv_vector_diff <- function(x, y) {
  x[match(x, y, 0L) == 0L]
}

renv_vector_intersect <- function(x, y) {
  y[match(x, y, 0L)]
}
