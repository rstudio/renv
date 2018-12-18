
renv_cache_entry <- function(record) {
  renv_paths_cache("install", record$Package, record$Hash, record$Package)
}

# TODO: allow users to enable / disable caching of packages
# find our newly-installed package
renv_cache_synchronize <- function(record) {

  # get path to library (bail if none recorded for this package)
  library <- renv_paths_library(record$Library) %||% ""
  if (!file.exists(library))
    return(FALSE)

  # get path to package in library (bail if doesn't exist)
  path <- file.path(library, record$Package)
  if (!file.exists(path))
    return(FALSE)

  # construct cache entry
  cache <- renv_cache_entry(record)

  # if our cache -> path link is already up-to-date, then nothing to do
  if (renv_file_same(cache, path))
    return(TRUE)

  # if we already have a cache entry, back it up
  callback <- renv_file_scoped_backup(cache)
  on.exit(callback(), add = TRUE)

  # copy into cache and link back into requested directory
  ensure_parent_directory(cache)
  renv_file_move(path, cache)
  renv_file_link(cache, path)

  TRUE

}
