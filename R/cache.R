
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

# given a record, find a compatible version of that package in the cache,
# using a computed hash if available; if no hash is available, then try
# to match based on the package name + version
renv_cache_find <- function(record) {

  # validate required fields -- if any are missing, we can't use the cache
  required <- c("Package", "Version")
  missing <- renv_vector_diff(required, names(record))
  if (length(missing))
    return("")

  # if we have a hash, use it directly
  if (!is.null(record$Hash)) {

    # generate path to package installations in cache
    paths <- with(record, renv_paths_cache(Package, Version, Hash, Package))

    # if there are multiple cache entries, return the first existing one
    # if no entries exist, return path into first cache entry
    for (path in paths)
      if (file.exists(path))
        return(path)

    return(paths[[1L]])

  }

  # if the record doesn't have a hash, check to see if we can still locate a
  # compatible package version within the cache
  root <- with(record, renv_paths_cache(Package, Version))
  hashes <- list.files(root, full.names = TRUE)
  packages <- list.files(hashes, full.names = TRUE)

  # iterate over package paths, read DESCRIPTION, and look
  # for something compatible with the requested record
  for (package in packages) {

    # try to read the DESCRIPTION file
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

    # check for compatible fields
    fields <- unique(c(
      renv_record_names(record, c("Package", "Version")),
      renv_record_names(dcf, c("Package", "Version"))
    ))

    # drop unnamed fields
    record <- record[nzchar(record)]
    dcf <- dcf[nzchar(dcf)]

    # check identical
    lhs <- keep(record, fields)
    rhs <- keep(dcf, fields)
    if (identical(lhs, rhs))
      return(package)

  }

  # failed; return "" as proxy for missing file
  ""

}

# given the path to a package's description file,
# compute the location it would be assigned if it
# were moved to the renv cache
renv_cache_path <- function(path) {
  record <- renv_description_read(path)
  record$Hash <- renv_hash_description(path)
  renv_cache_find(record)
}

renv_cache_path_components <- function(path) {

  data_frame(
    Package = renv_path_component(path, 1L),
    Hash    = renv_path_component(path, 2L),
    Version = renv_path_component(path, 3L)
  )

}

renv_cache_synchronize <- function(record, linkable = FALSE) {

  # construct path to package in library
  library <- renv_libpaths_active()
  path <- file.path(library, record$Package)
  if (!file.exists(path))
    return(FALSE)

  # bail if the package source is unknown
  # (packages with an unknown source are not cacheable)
  desc <- renv_description_read(path)
  source <- renv_snapshot_description_source(desc)
  if (identical(source, list(Source = "unknown")))
    return(FALSE)

  # bail if record not cacheable
  if (!renv_record_cacheable(record))
    return(FALSE)

  # if we don't have a hash, compute it now
  record$Hash <- record$Hash %||% renv_hash_description(path)

  # construct cache entry
  caches <- renv_cache_find(record)

  # try to synchronize
  copied <- FALSE
  for (cache in caches) {
    copied <- renv_cache_synchronize_impl(cache, record, linkable, path)
    if (copied)
      return(TRUE)
  }

  return(FALSE)

}

renv_cache_synchronize_impl <- function(cache, record, linkable, path) {

  # double-check we have a valid cache path
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
    file <- renv_scope_tempfile("renv-tempfile-", tmpdir = parent)
    status <- catchall(file.create(file))
    file.exists(file)
  })

  if (!writable)
    return(FALSE)

  # obtain lock on the cache
  lockpath <- file.path(parent, ".cache.lock")
  renv_scope_lock(lockpath)

  # if we already have a cache entry, back it up
  restore <- renv_file_backup(cache)
  defer(restore())

  # copy package from source location into the cache
  if (linkable) {
    renv_cache_move(path, cache, overwrite = TRUE)
    renv_file_link(cache, path, overwrite = TRUE)
  } else {
    renv_cache_copy(path, cache, overwrite = TRUE)
  }

  if (renv_platform_unix()) {

    # change the cache owner if set
    user <- Sys.getenv("RENV_CACHE_USER", unset = NA)
    if (!is.na(user)) {
      parent <- dirname(dirname(dirname(cache)))
      renv_system_exec(
        command = "chown",
        args    = c("-Rf", renv_shell_quote(user), renv_shell_path(parent)),
        action  = "chowning cached package",
        quiet   = TRUE,
        success = NULL
      )
    }

    # change file modes after copy if set
    mode <- Sys.getenv("RENV_CACHE_MODE", unset = NA)
    if (!is.na(mode)) {
      parent <- dirname(dirname(dirname(cache)))
      renv_system_exec(
        command = "chmod",
        args    = c("-Rf", renv_shell_quote(mode), renv_shell_path(parent)),
        action  = "chmoding cached package",
        quiet   = TRUE,
        success = NULL
      )
    }

    # finally, allow for an arbitrary callback if set
    callback <- getOption("renv.cache.callback")
    if (is.function(callback))
      callback(cache)

  }

  TRUE

}

renv_cache_list <- function(cache = NULL, packages = NULL) {
  caches <- cache %||% renv_paths_cache()
  paths <- map(caches, renv_cache_list_impl, packages = packages)
  unlist(paths, recursive = TRUE, use.names = FALSE)
}

renv_cache_list_impl <- function(cache, packages) {

  # paths to packages in the cache have the following format:
  #
  #    <package>/<version>/<hash>/<package>
  #
  # so find entries in the cache by listing files in each directory
  names <- file.path(cache, packages %||% list.files(cache))
  versions <- list.files(names, full.names = TRUE)
  hashes <- list.files(versions, full.names = TRUE)
  paths <- list.files(hashes, full.names = TRUE)

  # only keep paths that appear to be valid
  valid <- grep(renv_regexps_package_name(), basename(paths))
  paths[valid]

}

renv_cache_problems <- function(paths, reason) {

  data_frame(
    Package = renv_path_component(paths, 1L),
    Version = renv_path_component(paths, 3L),
    Path    = paths,
    Reason  = reason
  )

}

renv_cache_diagnose_corrupt_metadata <- function(paths, problems, verbose) {

  # check for missing metadata files
  metapaths <- file.path(paths, "Meta/package.rds")
  ok <- file.exists(metapaths)
  bad <- paths[!ok]

  if (length(bad)) {

    # nocov start
    if (verbose) {
      caution_bullets(
        "The following package(s) are missing 'Meta/package.rds':",
        renv_cache_format_path(bad),
        "These packages should be purged and reinstalled."
      )
    }
    # nocov end

    data <- renv_cache_problems(
      paths  = bad,
      reason = "'Meta/package.rds' does not exist"
    )

    problems$push(data)

  }

  # check for corrupt / unreadable metadata files
  ok <- map_lgl(metapaths, function(path) {
    rds <- catch(readRDS(path))
    !inherits(rds, "error")
  })

  bad <- paths[!ok]

  if (length(bad)) {

    # nocov start
    if (verbose) {
      caution_bullets(
        "The following package(s) have corrupt 'Meta/package.rds' files:",
        renv_cache_format_path(bad),
        "These packages should be purged and reinstalled."
      )
    }
    # nocov end

    data <- renv_cache_problems(
      paths  = bad,
      reason = "'Meta/package.rds' does not exist"
    )

    problems$push(data)

  }

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
    caution_bullets(
      "The following packages are missing DESCRIPTION files in the cache:",
      renv_cache_format_path(bad),
      "These packages should be purged and reinstalled."
    )
  }
  # nocov end

  data <- renv_cache_problems(
    paths  = bad,
    reason = "'DESCRIPTION' file does not exist"
  )

  problems$push(data)
  paths[exists]

}

renv_cache_diagnose_bad_hash <- function(paths, problems, verbose) {

  expected <- map_chr(paths, renv_cache_path)
  wrong <- paths != expected & !file.exists(expected)
  if (!any(wrong))
    return(paths)

  # nocov start
  if (verbose) {

    lhs <- renv_cache_path_components(paths[wrong])
    rhs <- renv_cache_path_components(expected[wrong])

    fmt <- "%s %s [Hash: %s != %s]"
    entries <- sprintf(fmt, lhs$Package, lhs$Version, lhs$Hash, rhs$Hash)

    caution_bullets(
      "The following packages have incorrect hashes:",
      entries,
      "Consider using `renv::rehash()` to re-hash these packages."
    )
  }
  # nocov end

  data <- renv_cache_problems(
    paths  = paths[wrong],
    reason = "unexpected hash"
  )

  problems$push(data)
  paths

}

renv_cache_diagnose_wrong_built_version <- function(paths, problems, verbose) {

  # form paths to DESCRIPTION files
  descpaths <- file.path(paths, "DESCRIPTION")

  # parse the version of R each was built for
  versions <- map_chr(descpaths, function(descpath) {

    tryCatch(
      renv_description_built_version(descpath),
      error = function(e) {
        warning(e)
        NA
      }
    )

  })

  # check for NAs, report and remove them
  isna <- is.na(versions)
  if (any(isna)) {

    # nocov start
    if (verbose) {

      caution_bullets(
        "The following packages have no 'Built' field recorded in their DESCRIPTION file:",
        paths[isna],
        "renv is unable to validate the version of R this package was built for."
      )

    }
    # nocov end

    data <- renv_cache_problems(
      paths = paths[isna],
      reason = "missing Built field"
    )

    problems$push(data)

    paths    <- paths[!isna]
    versions <- versions[!isna]

  }

  # check for incompatible versions
  wrong <- map_lgl(versions, function(version) {
    tryCatch(
      renv_version_compare(version, getRversion(), 2L) != 0,
      error = function(e) {
        warning(e)
        TRUE
      }
    )
  })

  if (!any(wrong))
    return(paths)

  # nocov start
  if (verbose) {

    caution_bullets(
      "The following packages in the cache were built for a different version of R:",
      renv_cache_format_path(paths[wrong]),
      "These packages will need to be purged and reinstalled."
    )

  }
  # nocov end

  data <- renv_cache_problems(
    paths = paths[wrong],
    reason = "built for different version of R"
  )

  problems$push(data)
  paths

}

renv_cache_diagnose <- function(verbose = NULL) {

  verbose <- verbose %||% renv_verbose()

  problems <- stack()
  paths <- renv_cache_list()
  paths <- renv_cache_diagnose_corrupt_metadata(paths, problems, verbose)
  paths <- renv_cache_diagnose_missing_descriptions(paths, problems, verbose)
  paths <- renv_cache_diagnose_bad_hash(paths, problems, verbose)
  paths <- renv_cache_diagnose_wrong_built_version(paths, problems, verbose)

  invisible(bind(problems$data()))

}

renv_cache_acls_reset <- function(target) {

  enabled <- Sys.getenv("RENV_CACHE_ACLS", unset = "TRUE")
  if (enabled)
    renv_acls_reset(target)

}

# copies a package at location 'source' to cache location 'target'
renv_cache_copy <- function(source, target, overwrite = FALSE) {
  ensure_parent_directory(target)
  renv_file_copy(source, target, overwrite = overwrite)
  renv_cache_acls_reset(target)
}

# moves a package from location 'source' to cache location 'target',
# and then links back from 'target' to 'source'
renv_cache_move <- function(source, target, overwrite = FALSE) {

  # move package into the cache if requested
  if (overwrite || !file.exists(target)) {
    ensure_parent_directory(target)
    renv_file_move(source, target, overwrite = TRUE)
  }

  # try to reset ACLs on the cache directory
  renv_cache_acls_reset(target)

  # link from the cache back to the target location
  renv_file_link(target, source, overwrite = TRUE)

}

# nocov start
renv_cache_format_path <- function(paths) {

  # extract path components
  names    <- format(renv_path_component(paths, 1L))
  hashes   <- format(renv_path_component(paths, 2L))
  versions <- format(renv_path_component(paths, 3L))

  # format and write
  fmt <- "%s %s [Hash: %s]"
  sprintf(fmt, names, versions, hashes)

}
# nocov end

renv_cache_clean_empty <- function(cache = NULL) {

  # no-op for Solaris
  if (renv_platform_solaris())
    return(FALSE)

  # move to cache root
  caches <- cache %||% renv_paths_cache()
  for (cache in caches)
    renv_cache_clean_empty_impl(cache)

  TRUE

}

renv_cache_clean_empty_impl <- function(cache) {

  # move to cache directory
  renv_scope_wd(cache)

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

renv_cache_config_enabled <- function(project) {
  config$cache.enabled() && settings$use.cache(project = project)
}

renv_cache_config_symlinks <- function(project) {

  usesymlinks <-
    config$cache.symlinks(default = NULL) %||%
    renv_cache_config_symlinks_default(project = project)

  usesymlinks && settings$use.cache(project = project)

}

renv_cache_config_symlinks_default <- function(project) {

  # on linux, we can always use symlinks
  if (renv_platform_unix())
    return(TRUE)

  # on Windows, only try to use symlinks (junction points) if the cache
  # and the project library appear to live on the same drive
  libpath <- renv_paths_library(project = project)
  cachepath <- renv_paths_cache()

  # TODO: with this change, anyone using networks not mapped to a local drive
  # would need to opt-in to using symlinks, but that's probably okay?
  all(
    substring(libpath, 1L, 2L) == substring(cachepath, 1L, 2L),
    substring(libpath, 2L, 2L) == ":",
    substring(cachepath, 2L, 2L) == ":"
  )


}

renv_cache_linkable <- function(project, library) {
  renv_cache_config_enabled(project = project) &&
    renv_cache_config_symlinks(project = project) &&
    getOption(
      "renv.cache.linkable",
      renv_path_same(library, renv_paths_library(project = project))
    )
}
