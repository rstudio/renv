
stopf <- function(fmt = "", ..., call. = FALSE) {
  stop(sprintf(fmt, ...), call. = call.)
}

warningf <- function(fmt = "", ..., call. = FALSE, immediate. = FALSE) {
  warning(sprintf(fmt, ...), call. = call., immediate. = immediate.)
}

printf <- function(fmt = "", ..., file = stdout(), sep = "") {
  if (!is.null(fmt) && renv_verbose())
    cat(sprintf(fmt, ...), file = file, sep = sep)
}

writef <- function(fmt = "", ..., con = stdout()) {
  if (!is.null(fmt) && renv_verbose())
    writeLines(sprintf(fmt, ...), con = con)
}

fyi <- function(fmt = NULL, ...) {
  prefix <- if (l10n_info()$`UTF-8`) "\u001b[34m\u2139\u001b[0m" else "i"
  message <- if (length(fmt)) sprintf(fmt, ...)
  if (length(message)) paste(prefix, message) else prefix
}

yay <- function(fmt = NULL, ...) {
  prefix <- if (l10n_info()$`UTF-8`) "\u001b[32m\u2714\u001b[0m" else "y"
  message <- if (length(fmt)) sprintf(fmt, ...)
  if (length(message)) paste(prefix, message) else prefix
}

boo <- function(fmt = NULL, ...) {
  prefix <- if (l10n_info()$`UTF-8`) "\u001b[31m\u2716\u001b[0m" else "x"
  message <- if (length(fmt)) sprintf(fmt, ...)
  if (length(message)) paste(prefix, message) else prefix
}
