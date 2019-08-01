
# tools for caching values read from a file, and invalidating those values if
# the file mtime changes. use `renv_filebacked_set()` to associate some value
# with a file at a particular point in time; `renv_filebacked_get()` will return
# that value, or NULL of the file mtime has changed
`_renv_filebacked` <- new.env(parent = emptyenv())

renv_filebacked_init <- function() {
  scopes <- c("DESCRIPTION", "settings")
  for (scope in scopes) {
    envir <- new.env(parent = emptyenv())
    assign(scope, envir, envir = `_renv_filebacked`)
  }
}

renv_filebacked_clear <- function(scope, path = NULL) {
  envir <- renv_filebacked_envir(scope)
  existing <- ls(envir = envir, all.names = TRUE)
  path <- path %||% existing
  removable <- intersect(path, existing)
  rm(list = removable, envir = envir)
}

renv_filebacked_set <- function(scope, path, value) {

  # validate the path
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  stopifnot(path_absolute(path))

  # create our cache entry
  info <- file.info(path, extra_cols = FALSE)
  entry <- list(value = value, exists = file.exists(path), mtime = info$mtime)

  # store it
  envir <- renv_filebacked_envir(scope)
  assign(path, entry, envir = envir)
  invisible(value)

}

renv_filebacked_get <- function(scope, path) {

  # validate the path
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  stopifnot(path_absolute(path))

  # get scoped sub-environment
  envir <- renv_filebacked_envir(scope)

  # check for entry in the cache
  if (!exists(path, envir = envir))
    return(NULL)

  entry <- get(path, envir = envir)
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

renv_filebacked_envir <- function(scope) {
  get(scope, envir = `_renv_filebacked`)
}
