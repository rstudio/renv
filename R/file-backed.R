
# tools for caching values read from a file, and invalidating those values if
# the file mtime changes. use `renv_filebacked_set()` to associate some value
# with a file at a particular point in time; `renv_filebacked_get()` will return
# that value, or NULL of the file mtime has changed
`_renv_filebacked_cache` <- new.env(parent = emptyenv())

renv_filebacked_set <- function(path, value) {

  if (!file.exists(path))
    return(FALSE)

  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  info <- file.info(path, extra_cols = FALSE)
  data <- list(value = value, mtime = info$mtime)

  assign(path, data, envir = `_renv_filebacked_cache`)
  invisible(value)

}

renv_filebacked_get <- function(path) {

  if (!file.exists(path))
    return(NULL)

  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  if (!exists(path, envir = `_renv_filebacked_cache`))
    return(NULL)

  entry <- get(path, envir = `_renv_filebacked_cache`)
  if (is.null(entry))
    return(NULL)

  info <- file.info(path, extra_cols = FALSE)
  if (is.na(entry$mtime) || is.na(info$mtime))
    return(NULL)

  if (info$mtime > entry$mtime)
    return(NULL)

  entry$value

}
