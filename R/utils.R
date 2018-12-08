stopf <- function(fmt, ..., call. = FALSE) {
  stop(sprintf(fmt, ...), call. = call.)
}

messagef <- function(fmt, ...) {
  message(sprintf(fmt, ...))
}

warningf <- function(fmt, ..., call. = FALSE) {
  warning(sprintf(fmt, ...), call. = call.)
}

is_scalar_character <- function(x) {
  is.character(x) && length(x) == 1
}

is_named <- function(x) {
  nm <- names(x)
  if (is.null(nm) || "" %in% nm)
    return(FALSE)
  TRUE
}

attempt <- function(expr) {
  tryCatch(expr, error = identity)
}

named <- function(object, names) {
  names(object) <- names
  object
}

empty <- function(x) {
  length(x) == 0
}

read <- function(file) {
  readChar(file, file.info(file)$size, TRUE)
}

aliased_path <- function(path) {
  home <- path.expand("~/")
  match <- regexpr(home, path, fixed = TRUE, useBytes = TRUE)
  if (identical(c(match), 1L))
    path <- paste("~", substring(path, nchar(home) + 1), sep = "/")
  path
}

ensure_directory <- function(path) {

  if (file.exists(path)) {

    info <- file.info(path)
    if (isTRUE(info$isdir))
      return(path)

    stopf("path '%s' exists but is not a directory", path)
  }

  if (!dir.create(path, recursive = TRUE))
    stopf("failed to create directory at path '%s'", path)

  invisible(path)

}

ensure_parent_directory <- function(path) {
  ensure_directory(dirname(path))
}

trimws <- function(x) {
  gsub("^\\s+|\\s+$", "", x)
}

pad_right <- function(text) {

  n <- nchar(text)
  diff <- max(n) - n

  spaces <- vapply(diff, function(d) {
    paste(rep.int(" ", d), collapse = "")
  }, character(1))

  paste(text, spaces, sep = "")
}

enumerate <- function(x, f, ...) {
  n <- names(x)
  result <- lapply(seq_along(x), function(i) {
    f(n[[i]], x[[i]], ...)
  })
  names(result) <- names(x)
  result
}

version_compatible <- function(lhs, rhs) {
  lhs <- unclass(lhs)[[1]]; rhs <- unclass(rhs)[[1]]
  n <- min(length(lhs), length(rhs))
  for (i in seq_len(n))
    if (lhs[[i]] != rhs[[i]])
      return(FALSE)
  return(TRUE)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

write_lines <- function(text, con) {
  if (is.null(con))
    return(text)
  writeLines(text, con = con, useBytes = TRUE)
}
