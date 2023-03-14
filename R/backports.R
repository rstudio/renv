
if (is.null(.BaseNamespaceEnv$dir.exists)) {

  dir.exists <- function(paths) {
    info <- renv_file_info(paths)
    info$isdir %in% TRUE
  }

}

if (is.null(.BaseNamespaceEnv$lengths)) {

  lengths <- function(x, use.names = TRUE) {
    vapply(x, length, numeric(1), USE.NAMES = use.names)
  }

}
