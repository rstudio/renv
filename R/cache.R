
# tools for interacting with the renv global package cache
renv_cache_version <- function() {
  "v3"
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

  # figure out the R version to be used when constructing
  # the cache package path
  built <- record$Built
  version <- if (is.null(built))
    getRversion()
  else
    substring(built, 3, regexpr(";", built, fixed = TRUE) - 1L)

  # if the record doesn't have a hash, check to see if we can still locate a
  # compatible package version within the cache
  root <- with(record, renv_paths_cache(Package, Version, version = version))
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
  packages <- all[file.exists(file.path(all, "DESCRIPTION"))]

  if (length(packages) == 0) {
    fmt <- "There are no packages within library '%s' to be copied."
    vwritef(fmt, aliased_path(library))
    return(0)
  }

  n <- length(packages)
  vwritef("* There are %i packages to synchronize with the cache.", n)
  if (!proceed()) {
    message("Operation aborted.")
    return(0)
  }

  vwritef("Copying packages into the cache ...")
  updates <- 0
  for (i in seq_along(packages)) {

    if (i %% 100 == 0)
      vwritef("... updating package %i of %i ...", i, n)

    package <- packages[[i]]

    record <- renv_description_read(package)

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
  vwritef(fmt, updates)

  updates

}

renv_cache_synchronize <- function(record, library = NULL, link = FALSE) {

  # construct path to package in library
  library <- library %||% renv_libpaths_default()
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
  if (link) {
    renv_file_move(path, cache)
    renv_file_link(cache, path, overwrite = TRUE)
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
  paths
}

renv_cache_diagnose_missing_descriptions <- function(paths, problems, verbose) {

  descpaths <- file.path(paths, "DESCRIPTION")
  info <- file.info(descpaths, extra_cols = FALSE)
  missing <- is.na(info$isdir)
  bad <- rownames(info)[missing]
  if (empty(bad))
    return(paths)

  if (verbose) {
    renv_pretty_print(
      renv_cache_format_path(dirname(bad)),
      "The following packages are missing DESCRIPTION files in the cache:",
      "These packages should be purged and re-installed.",
      wrap = FALSE
    )
  }

  path    <- dirname(bad)
  package <- path_component(bad, 1)
  version <- path_component(bad, 3)

  data <- data.frame(
    Package = package,
    Version = version,
    Path    = path,
    Reason  = "missing",
    stringsAsFactors = FALSE
  )

  problems$push(data)
  paths[!missing]

}

renv_cache_diagnose_bad_hash <- function(paths, problems, verbose) {

  hash <- path_component(paths, 2)
  computed <- map_chr(paths, renv_hash_description)
  diff <- hash != computed

  bad <- names(computed)[diff]
  if (empty(bad))
    return(paths)

  package <- path_component(bad, 1)
  version <- path_component(bad, 3)

  if (verbose) {

    fmt <- "%s %s [Hash: %s != %s]"
    entries <- sprintf(
      fmt,
      format(package),
      format(version),
      format(hash[diff]),
      format(computed[diff])
    )

    renv_pretty_print(
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

renv_cache_format_path <- function(paths) {

  names    <- format(path_component(paths, 1))
  hashes   <- format(path_component(paths, 2))
  versions <- format(path_component(paths, 3))

  fmt <- "%s %s [Hash: %s]"
  sprintf(fmt, names, versions, hashes)

}
