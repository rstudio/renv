
stopf <- function(fmt, ..., call. = FALSE) {
  stop(sprintf(fmt, ...), call. = call.)
}

warningf <- function(fmt, ..., call. = FALSE, immediate. = FALSE) {
  warning(sprintf(fmt, ...), call. = call., immediate. = immediate.)
}

messagef <- function(fmt, ..., appendLF = TRUE) {
  message(sprintf(fmt, ...), appendLF = appendLF)
}

printf <- function(fmt, ..., con = stdout()) {
  writeLines(sprintf(fmt, ...), con = con)
}
