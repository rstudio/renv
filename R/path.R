
path_absolute <- function(path) {
  grepl("^(?:[~/\\]|[a-zA-Z]:)", path)
}

path_within <- function(path, parent) {
  path <- path_canonicalize(path)
  parent <- path_canonicalize(parent)
  identical(parent, substring(path, 1, nchar(parent)))
}


# TODO: this relies on the parent directory of path existing
path_canonicalize <- function(path) {
  normalized <- normalizePath(dirname(path), winslash = "/", mustWork = TRUE)
  file.path(normalized, basename(path))
}

path_same <- function(lhs, rhs) {
  path_canonicalize(lhs) == path_canonicalize(rhs)
}
