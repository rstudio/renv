
# tools for caching values read from a file, and invalidating those values if
# the file mtime changes. use `renv_filebacked_set()` to associate some value
# with a file at a particular point in time; `renv_filebacked_get()` will return
# that value, or NULL of the file mtime has changed
`_renv_filebacked` <- new.env(parent = emptyenv())

renv_filebacked_init <- function() {
  scopes <- c("DESCRIPTION", "dependencies", "hash", "mran", "settings", "test")
  for (scope in scopes) {
    envir <- new.env(parent = emptyenv())
    assign(scope, envir, envir = `_renv_filebacked`)
  }
}

renv_filebacked_clear <- function(scope, path = NULL) {
  envir <- renv_filebacked_envir(scope)
  existing <- ls(envir = envir, all.names = TRUE)
  path <- path %||% existing
  removable <- renv_vector_intersect(path, existing)
  rm(list = removable, envir = envir)
}

renv_filebacked_set <- function(scope, path, value) {

  # validate the path
  stopifnot(renv_path_absolute(path))

  # create our cache entry
  info <- file.info(path, extra_cols = FALSE)
  entry <- list(value = value, info = info)

  # store it
  envir <- renv_filebacked_envir(scope)
  assign(path, entry, envir = envir)
  invisible(value)

}

renv_filebacked_get <- function(scope, path) {

  # validate the path
  stopifnot(renv_path_absolute(path))

  # get scoped sub-environment
  envir <- renv_filebacked_envir(scope)

  # check for entry in the cache
  entry <- envir[[path]]
  if (is.null(entry))
    return(NULL)

  # extract pieces of interest
  value   <- entry$value
  oldinfo <- entry$info
  newinfo <- file.info(path, extra_cols = FALSE)

  # if the file didn't exist when we set the entry,
  # check and see if it's still not there
  if (is.na(oldinfo$isdir) && is.na(newinfo$isdir))
    return(value)

  # compare on fields of interest
  fields <- c("size", "isdir", "mtime")
  if (!identical(oldinfo[fields], newinfo[fields]))
    return(NULL)

  # looks good
  value

}

renv_filebacked_envir <- function(scope) {
  get(scope, envir = `_renv_filebacked`)
}

renv_filebacked <- function(scope, path, callback, ...) {

  config <- config$filebacked.cache()
  if (identical(config, FALSE))
    return(callback(path, ...))

  cache <- renv_filebacked_get(scope, path)
  if (!is.null(cache))
    return(cache)

  result <- callback(path, ...)
  renv_filebacked_set(scope, path, result)
  result

}
