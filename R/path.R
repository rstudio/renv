
path_absolute <- function(path) {
  grepl("^(?:[~/\\]|[a-zA-Z]:)", path)
}

path_within <- function(path, parent) {
  path <- path.expand(path)
  parent <- path.expand(parent)
  identical(parent, substring(path, 1, nchar(parent)))
}
