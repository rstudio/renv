
# tools for caching values read from a file, and invalidating those values if
# the file mtime changes. use `renv_filebacked_set()` to associate some value
# with a file at a particular point in time; `renv_filebacked_get()` will return
# that value, or NULL of the file mtime has changed
`_renv_filebacked_cache` <- new.env(parent = emptyenv())

renv_filebacked_clear <- function(path = NULL) {
  existing <- ls(envir = `_renv_filebacked_cache`, all.names = TRUE)
  path <- path %||% existing
  removable <- intersect(existing, path)
  rm(list = removable, envir = `_renv_filebacked_cache`)
}

renv_filebacked_set <- function(path, value) {

  # validate the path
  path <- path.expand(path)
  stopifnot(path_absolute(path))

  # create our cache entry
  info <- file.info(path, extra_cols = FALSE)
  entry <- list(value = value, exists = file.exists(path), mtime = info$mtime)

  # store it
  assign(path, entry, envir = `_renv_filebacked_cache`)
  invisible(value)

}

renv_filebacked_get <- function(path) {

  # validate the path
  path <- path.expand(path)
  stopifnot(path_absolute(path))

  # check for entry in the cache
  if (!exists(path, envir = `_renv_filebacked_cache`))
    return(NULL)

  entry <- get(path, envir = `_renv_filebacked_cache`)
  if (is.null(entry))
    return(NULL)

  # if the file didn't exist when we set the entry,
  # check and see if it's still not there
  if (!entry$exists && !file.exists(path))
    return(entry$value)

  # check the file mtime (guard against NA)
  info <- file.info(path, extra_cols = FALSE)
  if (is.na(entry$mtime) || is.na(info$mtime))
    return(NULL)

  # if the file has been updated since, we have to
  # discard our old cached value
  if (info$mtime > entry$mtime)
    return(NULL)

  entry$value

}
