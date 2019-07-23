
sprintf <- function(fmt, ...) {

  dots <- eval(substitute(alist(...)))
  if (length(dots) == 0)
    return(fmt)

  base::sprintf(fmt, ...)

}

stopf <- function(fmt, ..., call. = FALSE) {
  stop(sprintf(fmt, ...), call. = call.)
}

warningf <- function(fmt, ..., call. = FALSE, immediate. = FALSE) {
  warning(sprintf(fmt, ...), call. = call., immediate. = immediate.)
}

messagef <- function(fmt, ..., appendLF = TRUE) {
  message(sprintf(fmt, ...), appendLF = appendLF)
}

printf <- function(fmt, ..., file = stdout()) {
  if (interactive() || !renv_testing())
    cat(sprintf(fmt, ...), file = file, sep = "")
}

writef <- function(fmt, ..., con = stdout()) {
  if (interactive() || !renv_testing())
    writeLines(sprintf(fmt, ...), con = con)
}

vmessagef <- function(fmt, ..., appendLF = TRUE) {
  if (renv_verbose())
    message(sprintf(fmt, ...), appendLF = appendLF)
}

vprintf <- function(fmt, ..., file = stdout()) {
  if (renv_verbose())
    cat(sprintf(fmt, ...), file = file, sep = "")
}

vwritef <- function(fmt, ..., con = stdout()) {
  if (renv_verbose())
    writeLines(sprintf(fmt, ...), con = con)
}
