
bapply <- function(x, f, ..., index = "Index") {
  result <- lapply(x, f, ...)
  bind_list(result, index = index)
}

enumerate <- function(x, f, ...) {
  n <- names(x); idx <- named(seq_along(x), n)
  lapply(idx, function(i) f(n[[i]], x[[i]], ...))
}

recurse <- function(x, f, ...) {
  f(x, ...)
  if (is.recursive(x))
    for (i in seq_along(x))
      recurse(x[[i]], f, ...)
}


uapply <- function(x, f, ...) {
  unlist(lapply(x, f, ...), recursive = FALSE)
}

map <- function(x, f, ...) {
  lapply(x, f, ...)
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

map_lgl <- function(x, f, ...) {
  vapply(x, f, ..., FUN.VALUE = logical(1))
}


extract <- function(x, ...) {
  lapply(x, `[[`, ...)
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

extract_lgl <- function(x, ...) {
  vapply(x, `[[`, ..., FUN.VALUE = logical(1))
}
