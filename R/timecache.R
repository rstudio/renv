
# tools for caching a value that should expire after
# some period of time
`_renv_timecache` <- new.env(parent = emptyenv())

renv_timecache <- function(key, value, limit = 3600, timeout = NULL) {

  # read cached time entry
  entry <- renv_timecache_get(key)

  # if the entry is null, this is our first time setting the cache
  if (is.null(entry)) {
    entry <- renv_timecache_set(key, value)
    return(entry$value)
  }

  # if it hasn't yet expired, we'll re-use the cached value
  if (!renv_timecache_expired(entry, limit))
    return(entry$value)

  # cache entry has expired -- run timeout hook if provided
  if (is.function(timeout))
    tryCatch(timeout(key), error = warning)

  # update the cache
  entry <- renv_timecache_set(key, value)
  entry$value

}

renv_timecache_get <- function(key) {
  id <- renv_deparse(key)
  if (exists(id, envir = `_renv_timecache`))
    get(id, envir = `_renv_timecache`, inherits = FALSE)
}

renv_timecache_set <- function(key, value) {
  id <- renv_deparse(key)
  entry <- renv_timecache_entry(value)
  assign(id, entry, envir = `_renv_timecache`, inherits = FALSE)
  entry
}

renv_timecache_entry <- function(value) {
  list(time = Sys.time(), value = value)
}

renv_timecache_expired <- function(entry, limit) {

  diff <- catch(difftime(Sys.time(), entry$time, units = "secs"))
  if (inherits(diff, "error") || diff > limit)
    return(TRUE)

  FALSE
}
