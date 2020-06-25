
# tools for querying information about packages available on CRAN.
# note that this does _not_ merge package entries from multiple repositories;
# rather, a list of databases is returned (one for each repository)
renv_available_packages <- function(type, limit = NULL, quiet = FALSE) {

  limit <- limit %||% Sys.getenv("R_AVAILABLE_PACKAGES_CACHE_CONTROL_MAX_AGE", "3600")

  # force a CRAN mirror when needed
  repos <- getOption("repos") %||% character()
  repos[repos == "@CRAN@"] <- "https://cloud.r-project.org"
  options(repos = convert(repos, "character"))

  renv_timecache(
    list(repos = getOption("repos"), type = type),
    renv_available_packages_impl(type, quiet),
    limit = as.integer(limit),
    timeout = renv_available_packages_timeout
  )
}

renv_available_packages_impl <- function(type, quiet = FALSE) {

  if (quiet)
    renv_scope_options(renv.verbose = FALSE)

  fmt <- "* Querying repositories for available %s packages ... "
  vprintf(fmt, type)

  # request repositories
  repos <- getOption("repos")
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
                                          filter = NULL,
                                          quiet  = FALSE)
{

  # if filter is a string, treat it as an explicit version requirement
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

  dbs <- renv_available_packages(type = type, quiet = quiet)
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

  fmt <- "failed to find %s for package %s in active repositories"
  stopf(fmt, type, package)

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

  record <- list(
    Package    = entry$Package,
    Version    = entry$Version,
    Source     = "Repository",
    Repository = entry$Name
  )

  attr(record, "type") <- type
  attr(record, "url")  <- entry$Repository

  record

}

renv_available_packages_latest_impl <- function(package, type) {

  dbs <- renv_available_packages(type = type, quiet = TRUE)
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
  entries[ordered[[1]], ]

}

renv_available_packages_latest <- function(package) {

  # if we're not using binary repositories,
  # then just take the latest available from source repositories
  types <- renv_package_pkgtypes()
  if (!"binary" %in% types) {

    entry <- renv_available_packages_latest_impl(package, "source")
    if (is.null(entry))
      stopf("package '%s' is not available", package)

    record <- renv_available_packages_record(entry, "source")
    return(record)

  }

  type <- getOption("pkgType")

  # detect requests for only source packages
  if (identical(type, "source"))
    return(renv_available_packages_latest_impl(package, "source"))

  # detect requests for only binary packages
  if (grepl("\\bbinary\\b", type))
    return(renv_available_packages_latest_impl(package, "binary"))

  # otherwise, check both source and binary repositories
  src <- renv_available_packages_latest_impl(package, "source")
  bin <- renv_available_packages_latest_impl(package, "binary")

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
  if (version_compare(bin$Version, src$Version) >= 0)
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
    if (!nzchar(Sys.getenv("MAKE", unset = "make")))
      ipcfs <- "never"

    if (identical(ipcfs, "never"))
      return(renv_available_packages_record(bin, "binary"))

  }

  # take the source version
  renv_available_packages_record(src, "source")

}
