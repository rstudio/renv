
renv_vector_diff <- function(x, y) {
  x[match(x, y, 0L) == 0L]
}

renv_vector_intersect <- function(x, y) {
  y[match(x, y, 0L)]
}

renv_vector_union <- function(x, y) {
  c(x, renv_vector_diff(y, x))
}

renv_vector_unique <- function(x) {
  x[!duplicated(x)]
}

renv_vector_merge <- function(x, y) {
  x[names(y)] <- y
  x
}
