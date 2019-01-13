
# tools for interacting with the renv global package cache
renv_cache_package_path <- function(record) {

  # if we have a hash, use it directly
  if (!is.null(record$Hash)) {
    path <- with(record, renv_paths_cache(Package, Version, Hash, Package))
    return(path)
  }

  # if we don't have a hash, check to see if we have a cache entry
  # for this version anyway, and use it if so
  hashes <- list.files(renv_paths_cache(record$Package, record$Version))
  if (length(hashes)) {
    path <- with(record, renv_paths_cache(Package, Version, hashes[[1]], Package))
    return(path)
  }

  # failed; return "" as proxy for missing file
  ""

}

# 'prime' the cache with the set of packages found in the user library
# (note: does not mutate input library)
renv_cache_prime <- function(library) {

  # list packages within the library
  all <- list.files(library, full.names = TRUE)

  # remove packages with no DESCRIPTION file
  packages <- all[file.exists(file.path(all, "DESCRIPTION"))]

  if (length(packages) == 0) {
    fmt <- "There are no packages within library '%s' to be copied."
    messagef(fmt, aliased_path(library))
    return(0)
  }

  n <- length(packages)
  messagef("* There are %i packages to synchronize with the cache.", n)
  response <- readline("Do you want to proceed? [Y/n]: ")
  if (!identical(tolower(response), "y")) {
    messagef("Operation aborted.")
    return(0)
  }

  messagef("Copying packages into the cache ...")
  updates <- 0
  for (i in seq_along(packages)) {

    if (i %% 100 == 0)
      messagef("... updating package %i of %i ...", i, n)

    package <- packages[[i]]

    record <- renv_description_read(package)
    record[["Hash"]] <- renv_hash_description(package)

    # construct cache entry
    cache <- renv_cache_package_path(record)

    # if we already have a cache entry, skip (assume up-to-date)
    if (file.exists(cache))
      next

    # if we already have a cache entry, back it up
    callback <- renv_file_scoped_backup(cache)
    on.exit(callback(), add = TRUE)

    # copy into cache and link back into requested directory
    ensure_parent_directory(cache)
    renv_file_copy(package, cache)

    updates <- updates + 1

  }

  fmt <- "%i package(s) were copied into the cache."
  messagef(fmt, updates)

  updates

}

# TODO: allow users to enable / disable caching of packages
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
  cache <- renv_cache_package_path(record)

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
