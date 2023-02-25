
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
  root <- renv_paths_common("index", c(), scope)
  index <- renv_index_load(root)

  # return index as-is when key is NULL
  if (is.null(key))
    return(index)

  # check for an index entry, and return it if it exists
  item <- renv_index_get(root, index, key, now, limit)
  if (!is.null(item))
    return(item)

  # otherwise, update the index
  renv_index_set(root, index, key, value, now, limit)

}

renv_index_load <- function(root) {
  tryCatch(
    expr    = renv_json_read(file.path(root, "index.json")),
    error   = function(e) NULL,
    warning = function(e) NULL
  )
}

renv_index_get <- function(root, index, key, now, limit) {

  # check for index entry
  entry <- index[[key]]
  if (is.null(entry))
    return(NULL)

  # see if it's expired
  if (renv_index_expired(entry, now, limit))
    return(NULL)

  data <- file.path(root, entry$data)
  if (!file.exists(data))
    return(NULL)

  readRDS(data)

}

renv_index_set <- function(root, index, key, value, now, limit) {

  # write data into index
  data <- tempfile("data-", tmpdir = root, fileext = ".rds")
  ensure_parent_directory(data)
  saveRDS(value, file = data, version = 2L)

  # clean up stale entries
  index <- renv_index_clean(root, index, limit, now)

  # update index file
  path <- file.path(root, "index.json")
  index <- if (file.exists(path)) renv_json_read(path)
  index[[key]] <- list(time = now, data = basename(data))
  ensure_parent_directory(path)
  renv_json_write(index, file = path)

  # return value
  invisible(value)

}

renv_index_encode <- function(key) {
  text <- deparse(key)
  renv_hash_text(text)
}

renv_index_clean <- function(root, index, now, limit) {

  # figure out what cache entries have expired
  expired <- map_lgl(index, renv_index_expired, now = now, limit = limit)

  # remove the expired cache entries
  paths <- map_chr(index, `[[`, "data")
  unlink(file.path(root, paths))

  # return the existing cache entries
  index[!expired]

}

renv_index_expired <- function(entry, now, limit) {
  now - entry$time > limit
}

renv_index_enabled <- function(scope, key) {
  getOption("renv.index.enabled", default = TRUE)
}
