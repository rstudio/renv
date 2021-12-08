
# tools for querying information about packages available on CRAN.
# note that this does _not_ merge package entries from multiple repositories;
# rather, a list of databases is returned (one for each repository)
renv_available_packages <- function(type, repos = NULL, limit = NULL, quiet = FALSE) {

  limit <- limit %||% Sys.getenv("R_AVAILABLE_PACKAGES_CACHE_CONTROL_MAX_AGE", "3600")
  repos <- renv_repos_normalize(repos %||% getOption("repos"))
  if (empty(repos))
    return(NULL)

  # invalidate cache if http_proxy or https_proxy environment variables change,
  # since those could effect (or even re-direct?) repository URLs
  envkeys <- c("http_proxy", "https_proxy", "HTTP_PROXY", "HTTPS_PROXY")
  envvals <- Sys.getenv(envkeys, unset = NA)

  # invalidate the cache if 'renv.download.headers' changes as well
  headers <- getOption("renv.download.headers")
  key <- list(repos = repos, type = type, headers = headers, envvals)

  timecache(
    key     = key,
    value   = renv_available_packages_impl(type, repos, quiet),
    limit   = as.integer(limit),
    timeout = renv_available_packages_timeout
  )

}

renv_available_packages_impl <- function(type, repos, quiet = FALSE) {

  if (quiet)
    renv_scope_options(renv.verbose = FALSE)

  fmt <- "* Querying repositories for available %s packages ... "
  vprintf(fmt, type)

  # request repositories
  urls <- contrib.url(repos, type)
  errors <- new.env(parent = emptyenv())
  dbs <- lapply(urls, renv_available_packages_query, errors = errors)
  names(dbs) <- names(repos)

  # notify finished
  vwritef("Done!")

  # propagate errors
  errors <- as.list(errors)
  enumerate(errors, function(url, output) {

    warnings <- output$warnings
    messages <- output$messages
    if (empty(warnings) && empty(messages))
      return()

    fmt <- "could not retrieve available packages for url %s"
    warningf(fmt, shQuote(url))

  })

  # filter results
  Filter(Negate(is.null), dbs)

}

renv_available_packages_query_packages_rds <- function(url) {
  path <- file.path(url, "PACKAGES.rds")
  destfile <- tempfile("renv-packages-", fileext = ".rds")
  download(url = path, destfile = destfile, quiet = TRUE)
  suppressWarnings(readRDS(destfile))
}

renv_available_packages_query_packages_gz <- function(url) {
  path <- file.path(url, "PACKAGES.gz")
  destfile <- tempfile("renv-packages-", fileext = ".gz")
  download(url = path, destfile = destfile, quiet = TRUE)
  suppressWarnings(read.dcf(destfile))
}

renv_available_packages_query_packages <- function(url) {
  path <- file.path(url, "PACKAGES")
  destfile <- tempfile("renv-packages-")
  download(url = path, destfile = destfile, quiet = TRUE)
  suppressWarnings(read.dcf(destfile))
}

renv_available_packages_query <- function(url, errors) {

  # check for a cached value
  name <- sprintf("repos_%s.rds.cache", URLencode(url, reserved = TRUE))
  path <- file.path(tempdir(), name)
  if (file.exists(path)) {
    db <- readRDS(path)
    unlink(path)
    return(as.data.frame(db, stringsAsFactors = FALSE))
  }

  # define query methods for the different PACKAGES
  methods <- list(
    renv_available_packages_query_packages_rds,
    renv_available_packages_query_packages_gz,
    renv_available_packages_query_packages
  )

  seize <- function(stack, restart) {
    function(condition) {
      stack$push(condition)
      invokeRestart(restart)
    }
  }

  warnings <- stack()
  messages <- stack()
  for (method in methods) {

    db <- withCallingHandlers(
      catch(method(url)),
      warning = seize(warnings, "muffleWarning"),
      message = seize(messages, "muffleMessage")
    )

    if (!inherits(db, "error"))
      return(renv_available_packages_success(db, url))

  }

  data <- list(
    warnings = warnings$data(),
    messages = messages$data()
  )

  assign(url, data, envir = errors)
  NULL

}

renv_available_packages_success <- function(db, url) {

  db <- as.data.frame(db, stringsAsFactors = FALSE)
  if (nrow(db) == 0)
    return(db)

  # remove packages which won't work on this OS
  ostype <- db$OS_type
  if (is.character(ostype)) {
    ok <- is.na(ostype) | ostype %in% .Platform$OS.type
    db <- db[ok, ]
  }


  # tag with repository
  db$Repository <- url

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
  dbs <- renv_available_packages(
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

renv_available_packages_timeout <- function(data) {
  urls <- contrib.url(data$repos, data$type)
  for (url in urls) {
    name <- sprintf("repos_%s.rds", URLencode(url, reserved = TRUE))
    path <- file.path(tempdir(), name)
    unlink(path)
  }
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

  attr(record, "type") <- type
  attr(record, "url")  <- entry$Repository

  record

}

renv_available_packages_latest_repos_impl <- function(package, type, repos) {

  # get available packages
  dbs <- renv_available_packages(
    type  = type,
    repos = repos,
    quiet = TRUE
  )

  # prepend local sources if available
  cellar <- renv_available_packages_cellar(type = type)
  if (!is.null(cellar)) {
    db <- list("__renv_cellar__" = cellar)
    dbs <- c(db, dbs)
  }

  fields <- c("Package", "Version", "OS_type", "NeedsCompilation", "Repository")
  entries <- bapply(dbs, function(db) {

    # extract entries for this package
    rows <- db[db$Package == package, ]
    if (nrow(rows) == 0L)
      return(rows)

    # only keep entries for which this version of R is compatible
    deps <- rows$Depends %||% rep.int("", nrow(rows))
    compatible <- map_lgl(deps, function(dep) {

      # skip NAs
      if (is.na(dep))
        return(TRUE)

      # read 'R' entries from Depends (if any)
      parsed <- catch(renv_description_parse_field(dep))
      if (inherits(parsed, "error")) {
        warning(parsed)
        return(FALSE)
      }

      # read requirements for R
      r <- parsed[parsed$Package == "R", ]
      if (nrow(r) == 0)
        return(TRUE)

      # build code to validate requirements
      fmt <- "getRversion() %s \"%s\""
      all <- sprintf(fmt, r$Require, r$Version)
      code <- paste(all, collapse = " && ")

      # evaluate it
      status <- catch(eval(parse(text = code), envir = baseenv()))
      if (inherits(status, "error")) {
        warning(status)
        return(TRUE)
      }

      # all done
      status

    })

    # keep only compatible rows + the required fields
    rows[compatible, intersect(fields, names(db))]

  }, index = "Name")

  if (is.null(entries))
    return(NULL)

  # sort based on version
  version <- numeric_version(entries$Version)
  ordered <- order(version, decreasing = TRUE)

  # return newest-available version
  entry <- as.list(entries[ordered[[1]], ])
  renv_available_packages_record(entry, type)

}

renv_available_packages_latest <- function(package,
                                           type = NULL,
                                           repos = NULL)
{
  methods <- list(
    renv_available_packages_latest_repos,
    renv_available_packages_latest_mran
  )

  errors <- stack()

  for (method in methods) {

    entry <- catch(method(package, type, repos))
    if (inherits(entry, "error")) {
      errors$push(entry)
      next
    }

    return(entry)

  }

  for (error in errors$data())
    warning(error)

  stopf("package '%s' is not available", package)
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
  keys <- ls(envir = entry)
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
  type <- type %||% getOption("pkgType")

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

  # find all files used in the locals folder
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

  # read the DESCRIPTION files within the archive
  descs <- lapply(keep, function(path) {

    # read the DESCRIPTION
    desc <- renv_description_read(path)

    # set the Repository field
    prefix <- if (renv_platform_windows()) "file:///" else "file://"
    uri <- paste0(prefix, dirname(path))
    desc[["Repository"]] <- uri

    # return it
    desc

  })

  # extract DESCRIPTION fields of interest
  fields <- c("Package", "Version", "OS_type", "NeedsCompilation", "Repository")
  records <- map(descs, function(desc) {

    # ensure missing fields are set as NA
    missing <- setdiff(fields, names(desc))
    desc[missing] <- NA

    # return record with requested fields
    desc[fields]

  })

  # bind into data.frame for lookup
  bind(records)

}
