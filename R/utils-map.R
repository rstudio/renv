
enumerate <- function(x, f, ...) {
  n <- names(x)
  result <- lapply(seq_along(x), function(i) {
    f(n[[i]], x[[i]], ...)
  })
  names(result) <- names(x)
  result
}

uapply <- function(x, f, ...) {
  unlist(lapply(x, f, ...), recursive = FALSE)
}

map_chr <- function(x, f, ...) {
  vapply(x, f, ..., FUN.VALUE = character(1))
}

map_dbl <- function(x, f, ...) {
  vapply(x, f, ..., FUN.VALUE = numeric(1))
}

map_int <- function(x, f, ...) {
  vapply(x, f, ..., FUN.VALUE = integer(1))
}


extract <- function(x, ...) {
  uapply(x, `[[`, ...)
}

extract_chr <- function(x, ...) {
  vapply(x, `[[`, ..., FUN.VALUE = character(1))
}

extract_dbl <- function(x, ...) {
  vapply(x, `[[`, ..., FUN.VALUE = numeric(1))
}

extract_int <- function(x, ...) {
  vapply(x, `[[`, ..., FUN.VALUE = integer(1))
}
