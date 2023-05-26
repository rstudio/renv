
sprintf <- function(fmt, ...) {

  if (nargs() == 1L)
    return(fmt)

  base::sprintf(fmt, ...)

}

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

vprintf <- function(fmt = "", ..., file = stdout(), sep = "") {
  if (!is.null(fmt) && renv_verbose())
    cat(sprintf(fmt, ...), file = file, sep = sep)
}

writef <- function(fmt = "", ..., con = stdout()) {
  if (!is.null(fmt) && renv_verbose())
    writeLines(sprintf(fmt, ...), con = con)
}

ewritef <- function(fmt = "", ..., con = stderr()) {
  if (!is.null(fmt) && renv_verbose())
    writeLines(sprintf(fmt, ...), con = con)
}

vwritef <- function(fmt = "", ..., con = stdout()) {
  if (!is.null(fmt) && renv_verbose())
    writeLines(sprintf(fmt, ...), con = con)
}

infof <- function(fmt = "", ..., con = stdout()) {
  if (!is.null(fmt) && renv_verbose()) {
    fmt <- paste(if (l10n_info()$`UTF-8`) "\u2139" else "i", fmt)
    writeLines(sprintf(fmt, ...), con = con)
  }
}
