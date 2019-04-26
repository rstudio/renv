
# tools for caching a value that should expire after
# some period of time
`_renv_timecache` <- new.env(parent = emptyenv())

renv_timecache <- function(key, value, limit = 3600) {
  entry <- renv_timecache_get(key)
  if (renv_timecache_expired(entry, limit))
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

  if (is.null(entry))
    return(TRUE)

  diff <- difftime(Sys.time(), entry$time, units = "secs")
  if (diff > limit)
    return(TRUE)

  FALSE
}
