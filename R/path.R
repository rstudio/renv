
path_absolute <- function(path) {
  grepl("^(?:[~/\\]|[a-zA-Z]:)", path)
}

path_within <- function(path, parent) {
  path <- path_canonicalize(path)
  parent <- paste(path_canonicalize(parent), "/", sep = "")
  identical(parent, substring(path, 1, nchar(parent)))
}

# TODO: this relies on the parent directory of path existing, but this
# is normally true in all the contexts where we use this function
path_canonicalize <- function(path) {
  parent <- dirname(path)
  if (!file.exists(parent))
    stopf("path '%s' does not exist", parent)
  root <- normalizePath(parent, winslash = "/", mustWork = TRUE)
  trimmed <- sub("/+$", "", root)
  file.path(trimmed, basename(path))
}

path_same <- function(lhs, rhs) {
  path_canonicalize(lhs) == path_canonicalize(rhs)
}
