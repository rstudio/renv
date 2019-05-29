
# tools for querying information about packages available on CRAN.
# note that this does _not_ merge package entries from multiple repositories;
# rather, a list of databases is returned (one for each repository)
renv_available_packages <- function(type, limit = NULL) {
  limit <- limit %||% Sys.getenv("R_AVAILABLE_PACKAGES_CACHE_CONTROL_MAX_AGE", "3600")
  renv_timecache(
    list(repos = getOption("repos"), type = type),
    renv_available_packages_impl(type),
    limit = as.integer(limit)
  )
}

renv_available_packages_impl <- function(type) {

  # force a CRAN mirror when needed
  repos <- getOption("repos") %||% character()
  repos[repos == "@CRAN@"] <- "https://cran.rstudio.com/"
  options(repos = repos)

  # notify user since this can take some time for non-local CRAN
  fmt <- "* Querying repositories for available %s packages ... "
  vprintf(fmt, type, file = stderr())

  # request available packages
  urls <- contrib.url(repos, type)
  result <- lapply(urls, renv_available_packages_query, type = type)

  # report success
  vwritef("Done!", con = stderr())
  result

}

renv_available_packages_query <- function(url, type) {

  # remove an old cached value
  name <- sprintf("repos_%s.rds", URLencode(url, reserved = TRUE))
  path <- file.path(tempdir(), name)
  unlink(path)

  # make the query (suppress warnings in case this is a local repository
  # whose PACKAGES files do not exist; note that an error is thrown in that
  # case anyhow)
  db <- withCallingHandlers(
    available.packages(contriburl = url),
    warning = function(w) invokeRestart("muffleWarning"),
    message = function(m) invokeRestart("muffleMessage")
  )

  # save to our cache
  ensure_parent_directory(path)
  saveRDS(db, file = path)

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
