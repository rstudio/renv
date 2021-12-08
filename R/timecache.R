
# tools for caching a value that should expire after
# some period of time
`_renv_timecache` <- stack(mode = "list")

timecache <- function(key, value, limit = 3600, timeout = NULL) {

  # allow override
  enabled <- getOption("renv.timecache.enabled")
  if (identical(enabled, FALSE))
    return(value)

  # read cached time entry
  entry <- renv_timecache_get(key)

  # if the entry is null, this is our first time setting the cache
  if (is.null(entry)) {
    entry <- renv_timecache_entry(key, value)
    renv_timecache_set(entry)
    return(entry$value)
  }

  # if it hasn't yet expired, we'll re-use the cached value
  if (!renv_timecache_expired(entry, limit))
    return(entry$value)

  # cache entry has expired -- run timeout hook if provided
  if (is.function(timeout))
    tryCatch(timeout(key), error = warning)

  # update the cache
  entry <- renv_timecache_entry(key, value)
  renv_timecache_set(entry)

  # return entry value
  entry$value

}

# returns the index of an existing cache entry (if any),
# or an index at which a new element could be inserted
renv_timecache_index <- function(key) {

  data <- `_renv_timecache`$data()

  for (index in seq_along(data)) {
    entry <- data[[index]]
    if (identical(key, entry$key))
      return(index)
  }

  length(data) + 1L

}

renv_timecache_get <- function(key) {
  index <- renv_timecache_index(key)
  `_renv_timecache`$get(index)
}

renv_timecache_set <- function(entry) {
  index <- renv_timecache_index(entry$key)
  `_renv_timecache`$set(index, entry)
}

renv_timecache_entry <- function(key, value) {
  list(time = Sys.time(), key = key, value = value)
}

renv_timecache_expired <- function(entry, limit) {

  diff <- catch(difftime(Sys.time(), entry$time, units = "secs"))
  if (inherits(diff, "error") || diff > limit)
    return(TRUE)

  FALSE

}
