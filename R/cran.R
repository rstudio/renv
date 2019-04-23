
# tools for querying information about packages available on CRAN.
# note that this does _not_ merge package entries from multiple repositories;
# rather, a list of databases is returned (one for each repository)
renv_available_packages <- function(type) {
  renv_timecache(
    list(repos = getOption("repos"), type = type),
    renv_available_packages_impl(type)
  )
}

renv_available_packages_impl <- function(type) {

  # force a CRAN mirror when needed
  repos <- getOption("repos") %||% character()
  repos[repos == "@CRAN@"] <- "https://cran.rstudio.com/"
  options(repos = repos)

  # request available packages
  urls <- contrib.url(repos, type)
  lapply(urls, renv_available_packages_query, type = type)

}

renv_available_packages_query <- function(url, type) {

  # check for cached value
  name <- sprintf("%s.rds", URLencode(url, reserved = TRUE))
  cache <- renv_paths_repos(name)
  if (!renv_file_exists(cache))
    return(renv_available_packages_query_impl(url, cache, type))

  # make sure the cache hasn't expired
  info <- file.info(cache, extra_cols = FALSE)
  diff <- difftime(Sys.time(), info$mtime, units = "secs")
  if (diff > 3600)
    return(renv_available_packages_query_impl(url, cache, type))

  # we have a live cache; use it
  db <- catch(readRDS(cache))
  if (inherits(db, "error"))
    return(renv_available_packages_query_impl(url, cache, type))

  as.data.frame(db, stringsAsFactors = FALSE)

}

renv_available_packages_query_impl <- function(url, cache, type) {

  # notify user since this can take some time for non-local CRAN
  if (!grepl("^file:", url)) {
    fmt <- "* Querying repositories for available %s packages -- please wait a moment ..."
    vwritef(fmt, type, con = stderr())
  }

  # make the query (suppress warnings in case this is a local repository
  # whose PACKAGES files do not exist; note that an error is thrown in that
  # case anyhow)
  db <- withCallingHandlers(
    available.packages(contriburl = url),
    warning = function(w) invokeRestart("muffleWarning"),
    message = function(m) invokeRestart("muffleMessage")
  )

  # save to our cache
  ensure_parent_directory(cache)
  saveRDS(db, file = cache)

  # return the db
  as.data.frame(db, stringsAsFactors = FALSE)

}

renv_available_packages_entry <- function(package, type, filter = NULL) {

  filter <- filter %||% function(entry) TRUE
  dbs <- renv_available_packages(type = type)
  for (db in dbs) {
    if (!package %in% db$Package)
      next

    entry <- db[package, ]
    if (filter(entry))
      return(entry)
  }

  stopf("package '%s' [%s] is not available", package, type)

}
