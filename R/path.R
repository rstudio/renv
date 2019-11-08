
renv_path_absolute <- function(path) {
  grepl("^(?:[~/\\]|[a-zA-Z]:)", path)
}

renv_path_within <- function(path, parent) {
  path <- renv_path_canonicalize(path)
  prefix <- paste(renv_path_canonicalize(parent), "/", sep = "")
  path == parent | substring(path, 1L, nchar(prefix)) == prefix
}

renv_path_normalize <- function(path, winslash = "/", mustWork = FALSE) {

  # NOTE: normalizePath() does not normalize path casing; e.g.
  # normalizePath("~/MyPaTh") will not normalize to "~/MyPath"
  # (assuming that is the "true" underlying casing on the filesystem)
  #
  # we work around this by round-tripping between the short name and
  # the long name, as Windows then has no choice but to figure out
  # the correct casing for us
  if (renv_platform_windows())
    path <- utils::shortPathName(path.expand(path))

  normalizePath(path, winslash = "/", mustWork = mustWork)

}

# TODO: this relies on the parent directory of path existing, but this
# is normally true in all the contexts where we use this function
renv_path_canonicalize <- function(path) {
  parent <- dirname(path)
  root <- renv_path_normalize(parent, winslash = "/", mustWork = TRUE)
  trimmed <- sub("/+$", "", root)
  file.path(trimmed, basename(path))
}

renv_path_same <- function(lhs, rhs) {
  renv_path_canonicalize(lhs) == renv_path_canonicalize(rhs)
}

# get the nth path component from the end of the path
renv_path_component <- function(path, index = 1) {
  splat <- strsplit(path, "[/\\]+")
  map_chr(splat, function(parts) parts[length(parts) - index + 1])
}

renv_path_pretty <- function(path) {
  shQuote(aliased_path(path), type = "cmd")
}
