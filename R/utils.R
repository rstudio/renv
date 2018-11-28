stopf <- function(fmt, ..., call. = FALSE) {
  stop(sprintf(fmt, ...), call. = call.)
}

messagef <- function(fmt, ...) {
  message(sprintf(fmt, ...))
}

warningf <- function(fmt, ...) {
  warning(sprintf(fmt, ...))
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

bimap <- function(...) {
  map <- list(...)
  map[as.character(map)] <- names(map)
  map
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
