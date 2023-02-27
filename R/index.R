
`_renv_index` <- new.env(parent = emptyenv())

index <- function(scope, key = NULL, value = NULL, limit = 3600L) {

  enabled <- renv_index_enabled(scope, key)
  if (!enabled)
    return(value)

  now <- as.integer(Sys.time())
  key <- if (!is.null(key)) renv_index_encode(key)

  tryCatch(
    renv_index_impl(scope, key, value, now, limit),
    error = function(e) value
  )

}

renv_index_impl <- function(scope, key, value, now, limit) {

  # load the index file
  root <- renv_paths_index(scope)
  index <- renv_index_load(root, scope)

  # return index as-is when key is NULL
  if (is.null(key))
    return(index)

  # check for an index entry, and return it if it exists
  item <- renv_index_get(root, scope, index, key, now, limit)
  if (!is.null(item))
    return(item)

  # otherwise, update the index
  renv_index_set(root, scope, index, key, value, now, limit)

}

renv_index_load <- function(root, scope) {
  tryCatch(
    expr    = renv_index_load_impl(root, scope),
    error   = function(e) NULL,
    warning = function(e) NULL
  )
}

renv_index_load_impl <- function(root, scope) {

  filebacked(
    scope = paste("index", scope, sep = "."),
    path = file.path(root, "index.json"),
    callback = function(path) renv_json_read(path)
  )

}

renv_index_get <- function(root, scope, index, key, now, limit) {

  # check for index entry
  entry <- index[[key]]
  if (is.null(entry))
    return(NULL)

  # see if it's expired
  if (renv_index_expired(entry, now, limit))
    return(NULL)

  # check for in-memory cached value
  value <- `_renv_index`[[scope]][[key]]
  if (!is.null(value))
    return(value)

  # otherwise, try to read from disk
  data <- file.path(root, entry$data)
  if (!file.exists(data))
    return(NULL)

  # read data from disk
  value <- readRDS(data)

  # add to in-memory cache
  `_renv_index`[[scope]] <-
    `_renv_index`[[scope]] %||%
    new.env(parent = emptyenv())

  `_renv_index`[[scope]][[key]] <- value

  # return value
  value

}

renv_index_set <- function(root, scope, index, key, value, now, limit) {

  # write data into index
  data <- tempfile("data-", tmpdir = root, fileext = ".rds")
  ensure_parent_directory(data)
  saveRDS(value, file = data, version = 2L)

  # clean up stale entries
  index <- renv_index_clean(root, scope, index, now, limit)

  # add index entry
  index[[key]] <- list(time = now, data = basename(data))

  # update index file
  path <- file.path(root, "index.json")
  ensure_parent_directory(path)
  renv_json_write(index, file = path)

  # return value
  value

}

renv_index_encode <- function(key) {
  text <- deparse(key)
  renv_hash_text(text)
}

renv_index_clean <- function(root, scope, index, now, limit) {

  # figure out what cache entries have expired
  ok <- enum_lgl(
    index,
    renv_index_clean_impl,
    root  = root,
    scope = scope,
    index = index,
    now   = now,
    limit = limit
  )

  # return the existing cache entries
  index[ok]

}

renv_index_clean_impl <- function(key, entry, root, scope, index, now, limit) {

  # check if cache entry has expired
  expired <- renv_index_expired(entry, now, limit)
  if (!expired)
    return(TRUE)

  # remove from in-memory cache
  cache <- `_renv_index`[[scope]]
  cache[[key]] <- NULL

  # remove from disk
  unlink(file.path(root, entry$data))

  FALSE

}

renv_index_expired <- function(entry, now, limit) {
  now - entry$time > limit
}

renv_index_enabled <- function(scope, key) {
  getOption("renv.index.enabled", default = TRUE)
}
