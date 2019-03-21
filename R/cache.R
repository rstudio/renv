
# tools for interacting with the renv global package cache
renv_cache_version <- function() {
  "v1"
}

renv_cache_package_path <- function(record) {

  # validate required fields -- if any are missing, we can't use the cache
  required <- c("Package", "Version")
  missing <- setdiff(required, names(record))
  if (length(missing))
    return("")

  # if we have a hash, use it directly
  if (!is.null(record$Hash)) {
    path <- with(record, renv_paths_cache(Package, Version, Hash, Package))
    return(path)
  }

  # if the record doesn't have a hash, check to see if we can still locate a
  # compatible package version within the cache
  root <- with(record, renv_paths_cache(Package, Version))
  hashes <- list.files(root, full.names = TRUE)
  packages <- list.files(hashes, full.names = TRUE)

  # iterate over package paths, read DESCRIPTION, and look
  # for something compatible with the requested record
  for (package in packages) {

    dcf <- catch(renv_description_read(package))
    if (inherits(dcf, "error"))
      next

    # if we're requesting an install from CRAN,
    # and the cached package has a "Repository" field,
    # then use it
    cran <-
      identical(record$Source, "CRAN") &&
      "Repository" %in% names(dcf)

    if (cran)
      return(package)

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
  packages <- all[renv_file_exists(file.path(all, "DESCRIPTION"))]

  if (length(packages) == 0) {
    fmt <- "There are no packages within library '%s' to be copied."
    messagef(fmt, aliased_path(library))
    return(0)
  }

  n <- length(packages)
  messagef("* There are %i packages to synchronize with the cache.", n)
  if (!proceed()) {
    message("Operation aborted.")
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

    # if we already have a cache entry, skip (assume up to date)
    if (renv_file_exists(cache))
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

renv_cache_synchronize <- function(record) {

  # construct path to package in library
  library <- renv_libpaths_default()
  path <- file.path(library, record$Package)
  if (!renv_file_exists(path))
    return(FALSE)

  # if we don't have a hash, compute it now
  record$Hash <- record$Hash %||% renv_hash_description(path)

  # construct cache entry
  cache <- renv_cache_package_path(record)

  # if our cache -> path link is already up to date, then nothing to do
  if (renv_file_same(cache, path))
    return(TRUE)

  # if we already have a cache entry, back it up
  callback <- renv_file_scoped_backup(cache)
  on.exit(callback(), add = TRUE)

  # copy into cache and link back into requested directory
  ensure_parent_directory(cache)
  if (library == renv_paths_library()) {
    renv_file_move(path, cache)
    renv_file_link(cache, path)
  } else {
    renv_file_copy(path, cache)
  }

  TRUE

}

renv_cache_list <- function(packages = NULL) {
  cache <- renv_paths_cache()
  names <- file.path(cache, packages %||% list.files(cache))
  versions <- list.files(names, full.names = TRUE)
  hashes <- list.files(versions, full.names = TRUE)
  paths <- list.files(hashes, full.names = TRUE)
  file.path(paths, "DESCRIPTION")
}

renv_cache_diagnose_missing_descriptions <- function(paths, problems, verbose) {

  info <- file.info(paths, extra_cols = FALSE)
  missing <- is.na(info$isdir)
  bad <- rownames(info)[missing]
  if (empty(bad))
    return(paths)

  fmt <- "%s %s\t[%s]"
  package <- path_component(bad, 2)
  version <- path_component(bad, 4)
  entries <- sprintf(fmt, package, version, aliased_path(dirname(bad)))

  if (verbose) {
    renv_pretty_print_packages(
      entries,
      "The following packages are missing DESCRIPTION files in the cache:",
      "These packages should be purged and re-installed.",
      wrap = FALSE
    )
  }

  data <- data.frame(
    Package = package,
    Version = version,
    Path    = dirname(bad),
    Reason  = "missing",
    stringsAsFactors = FALSE
  )

  problems$push(data)
  paths[!missing]

}

renv_cache_diagnose_bad_hash <- function(paths, problems, verbose) {

  hash <- path_component(paths, 3)
  computed <- map_chr(paths, renv_hash_description)
  diff <- hash != computed

  bad <- names(computed)[diff]
  if (empty(bad))
    return(paths)

  fmt <- "%s %s\t[%s != %s]"
  package <- path_component(bad, 2)
  version <- path_component(bad, 4)
  entries <- sprintf(fmt, package, version, hash[diff], computed[diff])

  if (verbose) {
    renv_pretty_print_packages(
      entries,
      "The following packages have incorrect hashes:",
      "These packages should be purged and re-installed.",
      wrap = FALSE
    )
  }

  data <- data.frame(
    Package = package,
    Version = version,
    Path    = dirname(bad),
    Reason  = "badhash",
    stringsAsFactors = FALSE
  )

  problems$push(data)
  paths

}

renv_cache_diagnose <- function(verbose = NULL) {

  verbose <- verbose %||% renv_verbose()

  problems <- stack()
  paths <- renv_cache_list()
  paths <- renv_cache_diagnose_missing_descriptions(paths, problems, verbose)
  paths <- renv_cache_diagnose_bad_hash(paths, problems, verbose)

  invisible(bind_list(problems$data()))

}

renv_cache_move <- function(source, target, overwrite = FALSE) {
  file.exists(source) || renv_file_move(target, source)
  renv_file_link(source, target, overwrite = TRUE)
}
