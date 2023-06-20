
# tools for caching values read from a file, and invalidating those values if
# the file mtime changes. use `renv_filebacked_set()` to associate some value
# with a file at a particular point in time; `renv_filebacked_get()` will return
# that value, or NULL of the file mtime has changed
the$filebacked_cache <- new.env(parent = emptyenv())

renv_filebacked_clear <- function(context, path = NULL) {

  # get cache associated with this context
  envir <- renv_filebacked_envir(context)

  # list all available cached results
  existing <- ls(envir = envir, all.names = TRUE)

  # if path is set, use it; otherwise remove everything
  path <- path %||% existing

  # validate the requested paths exist in the environment
  removable <- renv_vector_intersect(path, existing)

  # remove them
  rm(list = removable, envir = envir)
}

renv_filebacked_set <- function(context, path, value) {

  # validate the path
  stopifnot(renv_path_absolute(path))

  # create our cache entry
  info <- renv_file_info(path)
  entry <- list(value = value, info = info)

  # store it
  envir <- renv_filebacked_envir(context)
  assign(path, entry, envir = envir)
  invisible(value)

}

renv_filebacked_get <- function(context, path) {

  # validate the path
  if (!renv_path_absolute(path))
    stopf("internal error: '%s' is not an absolute path", path)

  # get contextd sub-environment
  envir <- renv_filebacked_envir(context)

  # check for entry in the cache
  entry <- envir[[path]]
  if (is.null(entry))
    return(NULL)

  # extract pieces of interest
  value   <- entry$value
  oldinfo <- entry$info
  newinfo <- renv_file_info(path)

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

renv_filebacked_envir <- function(context) {
  the$filebacked_cache[[context]] <-
    the$filebacked_cache[[context]] %||%
    new.env(parent = emptyenv())
}

filebacked <- function(context, path, callback, ...) {

  # don't use filebacked cache when disabled
  config <- config$filebacked.cache()
  if (identical(config, FALSE))
    return(callback(path, ...))

  # check for cache entry -- if available, use it
  cache <- renv_filebacked_get(context, path)
  if (!is.null(cache))
    return(cache)

  # otherwise, generate our value and cache it
  result <- callback(path, ...)
  renv_filebacked_set(context, path, result)

  result

}

renv_filebacked_invalidate <- function(path) {
  renv_scope_options(warn = -1L)
  eapply(the$filebacked_cache, function(context) {
    rm(list = path, envir = context)
  })
}
