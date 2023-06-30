
# tools for querying information about packages available on CRAN.
# note that this does _not_ merge package entries from multiple repositories;
# rather, a list of databases is returned (one for each repository)
available_packages <- function(type,
                               repos = NULL,
                               limit = NULL,
                               quiet = FALSE,
                               cellar = FALSE)
{
  dynamic(

    key = list(
      type = type,
      repos = repos %||% getOption("repos"),
      cellar = cellar
    ),

    value = renv_available_packages_impl(
      type   = type,
      repos  = repos,
      limit  = limit,
      quiet  = quiet,
      cellar = cellar
    )

  )
}

renv_available_packages_impl <- function(type,
                                         repos = NULL,
                                         limit = NULL,
                                         quiet = FALSE,
                                         cellar = FALSE)
{
  limit <- limit %||% Sys.getenv("R_AVAILABLE_PACKAGES_CACHE_CONTROL_MAX_AGE", "3600")
  repos <- renv_repos_normalize(repos %||% getOption("repos"))

  # invalidate cache if http_proxy or https_proxy environment variables change,
  # since those could effect (or even re-direct?) repository URLs
  envkeys <- c("http_proxy", "https_proxy", "HTTP_PROXY", "HTTPS_PROXY")
  envvals <- Sys.getenv(envkeys, unset = NA)

  # invalidate the cache if 'renv.download.headers' changes as well
  headers <- getOption("renv.download.headers")
  key <- list(repos = repos, type = type, headers = headers, envvals)

  # retrieve available packages
  dbs <- if (length(repos)) index(
    scope = "available-packages",
    key   = key,
    value = renv_available_packages_query(type, repos, quiet),
    limit = as.integer(limit)
  )

  # include cellar if requested
  dbs[["__renv_cellar__"]] <- if (cellar)
    renv_available_packages_cellar(type = type)

  dbs

}

renv_available_packages_query <- function(type, repos, quiet = FALSE) {

  if (quiet)
    renv_scope_options(renv.verbose = FALSE)

  fmt <- "- Querying repositories for available %s packages ... "
  printf(fmt, type)

  # exclude repositories which are known to not have packages available
  if (type == "binary") {
    ignored <- setdiff(grep("^BioC", names(repos), value = TRUE), "BioCsoft")
    repos <- repos[setdiff(names(repos), ignored)]
  }

  # request repositories
  urls <- contrib.url(repos, type)
  errors <- new.env(parent = emptyenv())
  dbs <- map(urls, renv_available_packages_query_impl, type = type, errors = errors)
  names(dbs) <- names(repos)

  # notify finished
  writef("Done!")

  # propagate errors
  errors <- as.list(errors)
  enumerate(errors, function(url, errors) {

    if (empty(errors))
      return()

    for (error in errors)
      warning(error)

    fmt <- "could not retrieve available packages for url %s"
    stopf(fmt, shQuote(url))

  })

  # filter results
  Filter(Negate(is.null), dbs)

}

renv_available_packages_query_impl_packages_rds <- function(url) {
  path <- file.path(url, "PACKAGES.rds")
  destfile <- renv_scope_tempfile("renv-packages-", fileext = ".rds")

  download(url = path, destfile = destfile, quiet = TRUE)
  suppressWarnings(readRDS(destfile))
}

renv_available_packages_query_impl_packages_gz <- function(url) {
  path <- file.path(url, "PACKAGES.gz")
  destfile <- renv_scope_tempfile("renv-packages-", fileext = ".gz")

  download(url = path, destfile = destfile, quiet = TRUE)
  suppressWarnings(read.dcf(destfile))
}

renv_available_packages_query_impl_packages <- function(url) {
  path <- file.path(url, "PACKAGES")
  destfile <- renv_scope_tempfile("renv-packages-")

  download(url = path, destfile = destfile, quiet = TRUE)
  suppressWarnings(read.dcf(destfile))
}

renv_available_packages_query_impl <- function(url, type, errors) {

  # define query_impl methods for the different PACKAGES
  methods <- list(
    renv_available_packages_query_impl_packages_rds,
    renv_available_packages_query_impl_packages_gz,
    renv_available_packages_query_impl_packages
  )

  stack <- stack()
  seize <- function(restart) {
    function(condition) {
      stack$push(condition)
      invokeRestart(restart)
    }
  }

  for (method in methods) {

    db <- withCallingHandlers(
      catch(method(url)),
      warning = seize(restart = "muffleWarning"),
      message = seize(restart = "muffleMessage")
    )

    if (inherits(db, "error")) {
      stack$push(db)
      next
    }

    return(renv_available_packages_success(db, url, type))

  }

  assign(url, stack$data(), envir = errors)
  NULL

}

renv_available_packages_success <- function(db, url, type) {

  # convert to data.frame
  db <- as_data_frame(db)
  if (nrow(db) == 0L)
    return(db)

  # build repository url
  repository <- rep.int(url, nrow(db))

  # update with path
  path <- db$Path
  if (length(path)) {
    set <- !is.na(path)
    repository[set] <- paste(url, path[set], sep = "/")
  }

  # set it
  db$Repository <- repository

  # add in necessary missing columns
  required <- c(
    "Package", "Version", "Priority",
    "Depends", "Imports", "LinkingTo", "Suggests", "Enhances",
    "License", "License_is_FOSS", "License_restricts_use",
    "OS_type", "Archs", "MD5sum",
    if (type %in% "source") "NeedsCompilation",
    "File", "Repository"
  )

  missing <- setdiff(required, names(db))
  db[missing] <- NA_character_
  db <- db[required]

  # filter as appropriate
  db <- renv_available_packages_filter(db)

  # remove row names
  row.names(db) <- NULL

  # ok
  db

}

renv_available_packages_entry <- function(package,
                                          type   = "source",
                                          repos  = NULL,
                                          filter = NULL,
                                          quiet  = FALSE,
                                          prefer = NULL)
{

  # if filter is a string, treat it as an explicit version requirement
  version <- NULL
  if (is.character(filter)) {
    version <- filter
    filter <- function(entries) {
      matches <- which(entries$Version == version)
      candidate <- head(matches, n = 1L)
      entries[candidate, ]
    }
  }

  # by default, provide a filter that selects the newest-available package
  filter <- filter %||% function(entries) {
    version <- numeric_version(entries$Version)
    ordered <- order(version, decreasing = TRUE)
    entries[ordered[[1]], ]
  }

  # read available packages
  dbs <- available_packages(
    type  = type,
    repos = repos,
    quiet = quiet
  )

  # if a preferred repository is marked and available, prefer using that
  if (length(prefer) == 1L && prefer %in% names(dbs)) {
    idx <- match(prefer, names(dbs))
    ord <- c(idx, setdiff(seq_along(dbs), idx))
    dbs <- dbs[ord]
  }

  # iterate through repositories, and find first matching
  for (i in seq_along(dbs)) {

    db <- dbs[[i]]
    matches <- which(db$Package == package)
    if (empty(matches))
      next

    entries <- db[matches, ]
    entry <- filter(entries)
    if (nrow(entry) == 0)
      next

    entry[["Type"]] <- type
    entry[["Name"]] <- names(dbs)[[i]] %||% ""
    return(entry)

  }

  # report package + version if both available
  pkgver <- if (length(version))
    paste(package, version)
  else
    package

  fmt <- "failed to find %s for '%s' in package repositories"
  stopf(fmt, type, pkgver)

}

renv_available_packages_record <- function(entry, type) {

  # check to see if this is already a proper record
  attrs <- attributes(entry)
  keys <- c("type", "url")
  if (all(keys %in% names(attrs)))
    return(entry)

  # otherwise, construct it
  record <- entry

  if (identical(record$Name, "__renv_cellar__")) {
    record$Source     <- "Cellar"
    record$Repository <- NULL
    record$Name       <- NULL
  } else {
    record$Source     <- "Repository"
    record$Repository <- entry$Name
    record$Name       <- NULL
  }

  # form url
  url <- entry$Repository
  path <- entry$Path
  if (length(path) && !is.na(path))
    url <- paste(url, path, sep = "/")

  attr(record, "type") <- type
  attr(record, "url")  <- url

  record

}

renv_available_packages_latest_repos_impl <- function(package, type, repos) {

  # get available packages
  dbs <- available_packages(
    type   = type,
    repos  = repos,
    quiet  = TRUE,
    cellar = TRUE
  )

  fields <- c(
    "Package", "Version",
    "OS_type", "NeedsCompilation",
    "Repository", "Path", "File"
  )

  entries <- bapply(dbs, function(db) {

    # extract entries for this package
    entries <- rows(db, db$Package == package)
    if (nrow(entries) == 0L)
      return(entries)

    # keep only compatible rows + the required fields
    cols(entries, intersect(fields, names(db)))

  }, index = "Name")

  if (is.null(entries))
    return(NULL)

  # sort based on version
  version <- numeric_version(entries$Version)
  ordered <- order(version, decreasing = TRUE)

  # extract newest entry
  entry <- as.list(entries[ordered[[1L]], ])

  # remove an NA file entry if necessary
  # https://github.com/rstudio/renv/issues/1045
  if (length(entry$File) && is.na(entry$File))
    entry$File <- NULL

  # return newest-available version
  renv_available_packages_record(entry, type)

}

renv_available_packages_latest <- function(package,
                                           type = NULL,
                                           repos = NULL)
{
  methods <- list(
    renv_available_packages_latest_repos,
    if (renv_mran_enabled())
      renv_available_packages_latest_mran
  )

  errors <- stack()

  entries <- lapply(methods, function(method) {

    if (is.null(method))
      return(NULL)

    entry <- catch(method(package, type, repos))
    if (inherits(entry, "error")) {
      errors$push(entry)
      return(NULL)
    }

    entry

  })

  # if both entries are null, error
  if (all(map_lgl(entries, is.null))) {
    map(errors$data(), warning)
    stopf("package '%s' is not available", package)
  } else if (is.null(entries[[2L]])) {
    return(entries[[1L]])
  } else if (is.null(entries[[1L]])) {
    return(entries[[2L]])
  }

  # extract both entries
  lhs <- entries[[1L]]
  rhs <- entries[[2L]]

  # extract versions
  lhsv <- package_version(lhs$Version %||% "0.0")
  rhsv <- package_version(rhs$Version %||% "0.0")

  # if the versions don't match, take the newest one
  if (lhsv > rhsv)
    return(lhs)
  else if (rhsv > lhsv)
    return(rhs)

  # otherwise, if we have a binary from the active package repositories,
  # use those; otherwise, use the mran binary
  if (identical(lhsv, rhsv)) {
    if (identical(attr(lhs, "type", exact = TRUE), "binary"))
      return(lhs)
    else
      return(rhs)
  }

  # otherwise, return the regular repository entry
  lhs

}

renv_available_packages_latest_mran <- function(package,
                                                type = NULL,
                                                repos = NULL)
{
  if (!config$mran.enabled())
    stop("MRAN is not enabled")

  type <- type %||% getOption("pkgType")
  if (identical(type, "source"))
    stop("MRAN database requires binary packages to be available")

  # ensure local MRAN database is up-to-date
  renv_mran_database_refresh(explicit = FALSE)

  # attempt to read it
  database <- catch(renv_mran_database_load())
  if (inherits(database, "error"))
    return(database)

  # get entry for this version of R + platform
  suffix <- contrib.url("", type = "binary")
  entry <- database[[suffix]]
  if (is.null(entry))
    stopf("no MRAN records available from repository URL '%s'", suffix)

  # find all available packages
  keys <- attr(entry, "keys")
  pattern <- paste0("^", package, " ")
  matching <- grep(pattern, keys, perl = TRUE, value = TRUE)
  if (empty(matching))
    stopf("package '%s' is not available from MRAN", package)

  # take the latest-available package
  entries <- unlist(mget(matching, envir = entry))
  sorted <- sort(entries, decreasing = TRUE)
  key <- names(sorted)[[1L]]
  idate <- sorted[[1L]]

  # split into package, version
  index <- regexpr(" ", key, fixed = TRUE)
  version <- substring(key, index + 1)

  # return an appropriate record
  record <- list(
    Package    = package,
    Version    = version,
    Source     = "Repository",
    Repository = "MRAN"
  )

  # convert from integer to date
  date <- as.Date(idate, origin = "1970-01-01")

  # form url to binary package
  base <- renv_mran_url(date, suffix)
  name <- renv_retrieve_name(record, type = "binary")
  url <- file.path(base, name)

  # tag record with url + type
  attr(record, "url")  <- dirname(url)
  attr(record, "type") <- "binary"

  record
}

renv_available_packages_latest_repos <- function(package,
                                                 type = NULL,
                                                 repos = NULL)
{
  type  <- type %||% getOption("pkgType")
  repos <- repos %||% getOption("repos")

  # detect requests for only source packages
  if (identical(type, "source"))
    return(renv_available_packages_latest_repos_impl(package, "source", repos))

  # detect requests for only binary packages
  if (grepl("\\bbinary\\b", type))
    return(renv_available_packages_latest_repos_impl(package, "binary", repos))

  # otherwise, check both source and binary repositories
  src <- renv_available_packages_latest_repos_impl(package, "source", repos)
  bin <- renv_available_packages_latest_repos_impl(package, "binary", repos)

  # choose an appropriate record
  if (is.null(src) && is.null(bin))
    stopf("package '%s' is not available", package)
  else if (is.null(src))
    renv_available_packages_record(bin, "binary")
  else if (is.null(bin))
    renv_available_packages_record(src, "source")
  else
    renv_available_packages_latest_select(src, bin)
}

renv_available_packages_latest_select <- function(src, bin) {

  # if the binary is at least as old as the source version,
  # then use the binary version
  if (renv_version_compare(bin$Version, src$Version) >= 0)
    return(renv_available_packages_record(bin, "binary"))

  # if the user has requested we skip source repositories,
  # use the binary anyway
  ipcs <- getOption("install.packages.check.source", default = "yes")
  if (!identical(ipcs, "yes"))
    return(renv_available_packages_record(bin, "binary"))

  # if the package requires compilation, check to see whether
  # the user has opted in to compiling packages from source
  nc <- identical(src$NeedsCompilation, "yes")
  if (nc) {

    # check user preference re: compilation from source
    ipcfs <- getOption(
      "install.packages.compile.from.source",
      default = Sys.getenv("R_COMPILE_AND_INSTALL_PACKAGES")
    )

    # if make is not available, then we can't build from source
    make <- Sys.getenv("MAKE", unset = "make")
    if (!nzchar(Sys.which(make)))
      ipcfs <- "never"

    # if we're on macOS and command line tools are not available,
    # then we can't build from sources
    if (renv_platform_macos() && !renv_xcode_available())
      ipcfs <- "never"

    if (identical(ipcfs, "never"))
      return(renv_available_packages_record(bin, "binary"))

  }

  # take the source version
  renv_available_packages_record(src, "source")

}

renv_available_packages_cellar <- function(type, project = NULL) {

  # look in the cellar
  project <- renv_project_resolve(project)
  roots <- renv_cellar_roots(project = project)

  # look for packages
  all <- list.files(
    path         = roots,
    all.files    = TRUE,
    full.names   = TRUE,
    recursive    = TRUE,
    include.dirs = FALSE
  )

  # keep only files with matching extensions
  ext <- renv_package_ext(type = type)
  keep <- all[fileext(all) %in% ext]

  # construct records for each cellar entry
  records <- lapply(keep, function(path) {

    # infer package name, version from tarball name
    base <- basename(keep)
    idx <- regexpr("_", base, fixed = TRUE)
    package <- substring(base, 1L, idx - 1L)
    version <- substring(base, idx + 1L, nchar(base) - nchar(ext))

    # set the Repository field
    prefix <- if (renv_platform_windows()) "file:///" else "file://"
    repository <- paste0(prefix, dirname(path))

    # build record
    list(
      Package = package,
      Version = version,
      Repository = repository
    )

  })

  bind(records)

}

renv_available_packages_filter <- function(db) {

  # sanity check
  if (is.null(db) || nrow(db) == 0L)
    return(db)

  # TODO: subarch? duplicates?
  # remove packages which won't work on this OS
  db <- renv_available_packages_filter_ostype(db)
  db <- renv_available_packages_filter_version(db)

  # return filtered database
  db

}

renv_available_packages_filter_ostype <- function(db) {
  ostype <- db$OS_type
  ok <- is.na(ostype) | ostype %in% .Platform$OS.type
  rows(db, ok)
}

renv_available_packages_filter_version <- function(db) {

  depends <- db$Depends

  # find the packages which express an R dependency
  splat <- strsplit(depends, "\\s*,\\s*", perl = TRUE)

  # remove the non-R dependencies
  table <- c("R ", "R\n", "R(")
  splat <- map(splat, function(requirements) {
    requirements[match(substr(requirements, 1L, 2L), table, 0L) != 0L]
  })

  # collect the unique R dependencies
  dependencies <- unique(unlist(splat))

  # convert this to a simpler form
  pattern <- "^R\\s*\\(([^\\d\\s+]+)\\s*([^\\)]+)\\)$"
  matches <- gsub(pattern, "\\1 \\2", dependencies, perl = TRUE)

  # split into operator and version
  idx <- regexpr(" ", matches, fixed = TRUE)
  ops <- substring(matches, 1L, idx - 1L)
  version <- numeric_version(substring(matches, idx + 1L))

  # bundle the calls for efficiency
  ok <- rep.int(NA, length(ops))
  names(ok) <- dependencies

  # iterate over the operations, and update our vector
  rversion <- getRversion()
  for (op in unique(ops)) {
    idx <- ops == op
    ok[idx] <- do.call(op, list(rversion, version[idx]))
  }

  # now, map the names back to their computed values, and check whether
  # all requirements were satisfied
  ok <- map_lgl(splat, function(requirements) {
    all(ok[requirements])
  })

  rows(db, ok)

}

# flattens available packages, keeping only the newest version
renv_available_packages_flatten <- function(dbs) {

  # stack the databases together
  stacked <- bind(dbs)

  # order by package + version
  # TODO: 'order()' is kind of slow for numeric versions; can we do better?
  index <- with(stacked, order(Package, numeric_version(Version), decreasing = TRUE))
  ordered <- rows(stacked, index)

  # remove duplicates
  dupes <- duplicated(ordered$Package)
  filtered <- rows(ordered, !dupes)

  # ready to return
  filtered

}
