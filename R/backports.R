
if (is.null(.BaseNamespaceEnv$isNamespaceLoaded)) {

  isNamespaceLoaded <- function(name) {
    name %in% loadedNamespaces()
  }

}

if (is.null(.BaseNamespaceEnv$dir.exists)) {

  dir.exists <- function(paths) {
    info <- suppressWarnings(file.info(paths, extra_cols = FALSE))
    info$isdir %in% TRUE
  }

}

if (is.null(.BaseNamespaceEnv$lengths)) {

  lengths <- function(x, use.names = TRUE) {
    vapply(x, length, numeric(1), USE.NAMES = use.names)
  }

}

if (is.null(.BaseNamespaceEnv$startsWith)) {

  startsWith <- function(x, prefix) {
    pattern <- sprintf("^\\Q%s\\E", prefix)
    grepl(pattern, x, perl = TRUE)
  }

}

if (is.null(.BaseNamespaceEnv$endsWith)) {

  endsWith <- function(x, suffix) {
    pattern <- sprintf("\\Q%s\\E$", suffix)
    grepl(pattern, x, perl = TRUE)
  }

}

