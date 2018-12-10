
stopf <- function(fmt, ..., call. = FALSE) {
  stop(sprintf(fmt, ...), call. = call.)
}

messagef <- function(fmt, ..., appendLF = TRUE) {
  message(sprintf(fmt, ...), appendLF = appendLF)
}

warningf <- function(fmt, ..., call. = FALSE) {
  warning(sprintf(fmt, ...), call. = call.)
}

