
# tools for interacting with the renv global package cache
renv_cache_version <- function() {
  # NOTE: users should normally not override the cache version;
  # this is provided just to make testing easier
  Sys.getenv("RENV_CACHE_VERSION", unset = "v5")
}

renv_cache_version_previous <- function() {
  version <- renv_cache_version()
  number <- as.integer(substring(version, 2L))
  paste("v", number - 1L, sep = "")
}

renv_cache_package_path <- function(record) {

  # validate required fields -- if any are missing, we can't use the cache
  required <- c("Package", "Version")
  missing <- renv_vector_diff(required, names(record))
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

    dcf <- catch(as.list(renv_description_read(package)))
    if (inherits(dcf, "error"))
      next

    # if we're requesting an install from an R package repository,
    # and the cached package has a "Repository" field, then use it
    source <- renv_record_source(record)
    hasrepo <-
      source %in% c("cran", "repository") &&
      "Repository" %in% names(dcf)

    if (hasrepo)
      return(package)

    # otherwise, match on other fields
    fields <- renv_record_names(record, c("Package", "Version"))

    # drop unnamed fields
    record <- record[nzchar(record)]; dcf <- dcf[nzchar(dcf)]

    # check identical
    if (identical(record[fields], dcf[fields]))
      return(package)

  }

  # failed; return "" as proxy for missing file
  ""

}

renv_cache_synchronize <- function(record, linkable = FALSE) {

  # construct path to package in library
  library <- renv_libpaths_default()
  path <- file.path(library, record$Package)
  if (!file.exists(path))
    return(FALSE)

  # bail if the package source is unknown (assume that packages with an
  # unknown source are not cacheable)
  desc <- renv_description_read(path)
  source <- renv_snapshot_description_source(desc)
  if (identical(source, list(Source = "Unknown")))
    return(FALSE)

  # bail if record not cacheable
  if (!renv_record_cacheable(record))
    return(FALSE)

  # if we don't have a hash, compute it now
  record$Hash <- record$Hash %||% renv_hash_description(path)

  # construct cache entry
  cache <- renv_cache_package_path(record)
  if (!nzchar(cache))
    return(FALSE)

  # if our cache -> path link is already up to date, then nothing to do
  if (renv_file_same(cache, path))
    return(TRUE)

  # try to create the cache directory target
  # (catch errors due to permissions, etc)
  parent <- dirname(cache)
  status <- catchall(ensure_directory(parent))
  if (inherits(status, "error"))
    return(FALSE)

  # double-check that the cache is writable
  writable <- local({
    file <- tempfile("renv-tempfile-", tmpdir = parent)
    on.exit(unlink(file, force = TRUE), add = TRUE)
    status <- catchall(file.create(file))
    file.exists(file)
  })

  if (!writable)
    return(FALSE)

  # if we already have a cache entry, back it up
  callback <- renv_file_backup(cache)
  on.exit(callback(), add = TRUE)

  # copy into cache and link back into requested directory
  if (linkable) {
    renv_file_move(path, cache)
    renv_file_link(cache, path, overwrite = TRUE)
  } else {
    vprintf("* Copying '%s' into the cache ... ", record$Package)
    renv_file_copy(path, cache)
    vwritef("Done!")
  }

  TRUE

}

renv_cache_list <- function(cache = NULL, packages = NULL) {
  cache <- cache %||% renv_paths_cache()
  names <- file.path(cache, packages %||% list.files(cache))
  versions <- list.files(names, full.names = TRUE)
  hashes <- list.files(versions, full.names = TRUE)
  paths <- list.files(hashes, full.names = TRUE)
  paths
}

renv_cache_diagnose_missing_descriptions <- function(paths, problems, verbose) {

  descpaths <- file.path(paths, "DESCRIPTION")
  exists <- file.exists(descpaths)
  bad <- paths[!exists]
  if (empty(bad))
    return(paths)

  # nocov start
  if (verbose) {
    renv_pretty_print(
      renv_cache_format_path(dirname(bad)),
      "The following packages are missing DESCRIPTION files in the cache:",
      "These packages should be purged and re-installed.",
      wrap = FALSE
    )
  }
  # nocov end

  path    <- dirname(bad)
  package <- renv_path_component(bad, 1)
  version <- renv_path_component(bad, 3)

  data <- data.frame(
    Package = package,
    Version = version,
    Path    = path,
    Reason  = "missing",
    stringsAsFactors = FALSE
  )

  problems$push(data)
  paths[exists]

}

renv_cache_diagnose_bad_hash <- function(paths, problems, verbose) {

  hash <- renv_path_component(paths, 2)
  computed <- map_chr(paths, renv_hash_description)
  diff <- hash != computed

  bad <- names(computed)[diff]
  if (empty(bad))
    return(paths)

  package <- renv_path_component(bad, 1)
  version <- renv_path_component(bad, 3)

  # nocov start
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
      "Consider using `renv::rehash()` to re-hash these packages.",
      wrap = FALSE
    )
  }
  # nocov end

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

# nocov start
renv_cache_format_path <- function(paths) {

  names    <- format(renv_path_component(paths, 1))
  hashes   <- format(renv_path_component(paths, 2))
  versions <- format(renv_path_component(paths, 3))

  fmt <- "%s %s [Hash: %s]"
  sprintf(fmt, names, versions, hashes)

}
# nocov end

renv_cache_clean_empty <- function(cache = NULL) {

  # no-op for Solaris
  if (renv_platform_solaris())
    return(FALSE)

  # move to cache root
  cache <- cache %||% renv_paths_cache()
  owd <- setwd(cache)
  on.exit(setwd(owd), add = TRUE)

  # construct system command for removing empty directories
  action <- "removing empty directories"
  if (renv_platform_windows()) {
    args <- c(".", ".", "/S", "/MOVE")
    renv_system_exec("robocopy", args, action, 0:8)
  } else {
    args <- c(".", "-type", "d", "-empty", "-delete")
    renv_system_exec("find", args, action)
  }

  TRUE

}

renv_cache_package_validate <- function(path) {

  if (renv_project_type(path) == "package")
    return(TRUE)

  type <- renv_file_type(path, symlinks = FALSE)
  if (!nzchar(type))
    return(FALSE)

  name <- if (type == "directory") "directory" else "file"
  fmt <- "%s %s exists but does not appear to be an R package"
  warningf(fmt, name, shQuote(path))

  FALSE

}
