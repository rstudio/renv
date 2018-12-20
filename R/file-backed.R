
# tools for caching values read from a file, and invalidating those
# values if the file mtime changes
`_renv_filebacked_cache` <- new.env(parent = emptyenv())

renv_filebacked_set <- function(path, value) {

  if (!file.exists(path))
    return(FALSE)

  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  info <- file.info(path, extra_cols = FALSE)
  data <- list(mtime = info$mtime, value = value)

  assign(path, data, envir = `_renv_filebacked_cache`)
  invisible(value)

}

renv_filebacked_get <- function(path) {

  if (!file.exists(path))
    return(NULL)

  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  if (!exists(path, envir = `_renv_filebacked_cache`))
    return(NULL)

  cache <- get(path, envir = `_renv_filebacked_cache`)
  if (is.null(cache))
    return(NULL)

  info <- file.info(path, extra_cols = FALSE)
  if (info$mtime > cache$mtime)
    return(NULL)

  cache$value

}
