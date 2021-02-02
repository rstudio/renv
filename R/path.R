
renv_path_absolute <- function(path) {
  grepl("^(?:[~/\\]|[a-zA-Z]:)", path)
}

renv_path_within <- function(path, parent) {
  path <- renv_path_canonicalize(path)
  prefix <- paste(renv_path_canonicalize(parent), "/", sep = "")
  path == parent | substring(path, 1L, nchar(prefix)) == prefix
}

renv_path_normalize_win32 <- function(path,
                                      winslash = "/",
                                      mustWork = FALSE)
{
  map_chr(
    path,
    renv_path_normalize_win32_impl,
    winslash = winslash,
    mustWork = mustWork
  )
}

renv_path_normalize <- function(path, winslash = "/", mustWork = FALSE) {
  renv_methods_error()
}

# NOTE: in versions of R < 4.0.0, normalizePath() does not normalize path
# casing; e.g. normalizePath("~/MyPaTh") will not normalize to "~/MyPath"
# (assuming that is the "true" underlying casing on the filesystem)
#
# we work around this by round-tripping between the short name and
# the long name, as Windows then has no choice but to figure out
# the correct casing for us
#
# this isn't 100% reliable (not all paths have a short-path equivalent)
# but seems to be good enough in practice ...
#
# except that, if the path contains characters not representable in the
# current encoding, then attempting to normalize the short version of
# that path will fail -- so if the path is already UTF-8, then we need to
# avoid round-tripping through the short path
#
# https://github.com/rstudio/renv/issues/629
renv_path_normalize_win32 <- function(path,
                                      winslash = "/",
                                      mustWork = FALSE)
{
  utf8 <- Encoding(path) == "UTF-8"
  path[utf8]  <- normalizePath(path[utf8], winslash, mustWork)
  path[!utf8] <- renv_path_normalize_win32_impl(path[!utf8], winslash, mustWork)
  path
}

renv_path_normalize_win32_impl <- function(path,
                                           winslash = "/",
                                           mustWork = FALSE)
{
  short <- utils::shortPathName(path.expand(path))
  normalizePath(short, winslash, mustWork)
}

# TODO: this is a lie; for existing paths symlinks will be resolved
renv_path_canonicalize <- function(path) {
  parent <- dirname(path)
  root <- renv_path_normalize(parent, winslash = "/", mustWork = FALSE)
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
