
renv_mran_enabled <- function() {
  !identical(getOption("pkgType"), "source") && config$mran.enabled()
}

renv_mran_database_path <- function() {
  renv_paths_mran("packages.rds")
}

renv_mran_database_encode <- function(database) {
  database <- as.list(database)
  encoded <- lapply(database, renv_mran_database_encode_impl)
  encoded[order(names(encoded))]
}

renv_mran_database_encode_impl <- function(entry) {

  entry <- as.list(entry)
  keys <- names(entry)
  vals <- unlist(entry)

  splat <- strsplit(keys, " ", fixed = TRUE)

  encoded <- data_frame(
    Package          = map_chr(splat, `[[`, 1L),
    Version          = map_chr(splat, `[[`, 2L),
    Date             = as.integer(vals)
  )

  encoded <- encoded[order(encoded$Package, encoded$Version), ]
  rownames(encoded) <- NULL

  encoded$Package <- as.factor(encoded$Package)
  encoded$Version <- as.factor(encoded$Version)

  encoded

}

renv_mran_database_decode <- function(encoded) {
  decoded <- lapply(encoded, renv_mran_database_decode_impl)
  list2env(decoded, parent = emptyenv())
}

renv_mran_database_decode_impl <- function(entry) {

  entry$Package <- as.character(entry$Package)
  entry$Version <- as.character(entry$Version)

  keys <- paste(entry$Package, entry$Version)
  vals <- as.list(entry$Date)
  names(vals) <- keys

  envir <- list2env(vals, parent = emptyenv())
  attr(envir, "keys") <- keys

  envir

}

renv_mran_database_save <- function(database, path = NULL) {

  path <- path %||% renv_mran_database_path()
  ensure_parent_directory(path)

  encoded <- renv_mran_database_encode(database)

  conn <- xzfile(path)
  defer(close(conn))
  saveRDS(encoded, file = conn, version = 2L)

}

renv_mran_database_load <- function(path = NULL) {

  filebacked(
    context  = "renv_mran_database_load",
    path     = path %||% renv_mran_database_path(),
    callback = renv_mran_database_load_impl
  )

}

renv_mran_database_load_impl <- function(path) {

  # read from database file if it exists
  if (file.exists(path)) {
    encoded <- readRDS(path)
    return(renv_mran_database_decode(encoded))
  }

  # otherwise, initialize a new database
  new.env(parent = emptyenv())

}

renv_mran_database_dates <- function(version, all = TRUE) {

  # release dates for old versions of R
  releases <- c(
    "3.2" = "2015-04-16",
    "3.3" = "2016-05-03",
    "3.4" = "2017-04-21",
    "3.5" = "2018-04-23",
    "3.6" = "2019-04-26",
    "4.0" = "2020-04-24",
    "4.1" = "2021-05-18",
    "4.2" = "2022-04-22",
    "4.3" = "2023-05-18",  # a guess
    "4.4" = "2024-05-18",  # a guess
    NULL
  )

  # find the start date
  index <- match(version, names(releases))
  if (is.na(index))
    stopf("no known release date for R %s", version)

  start <- as.Date(releases[[index]])
  if (!all)
    return(start)

  # form end date (ensure not in future)
  # we look 2 releases in the future as R builds binaries for
  # the previous releases of R as well
  end <- min(
    as.Date(releases[[index + 2L]]),
    as.Date(Sys.time(), tz = "UTC")
  )

  # generate list of dates
  seq(start, end, by = 1L)

}

renv_mran_database_key <- function(platform, version) {
  sprintf("/bin/%s/contrib/%s", platform, version)
}

renv_mran_database_update <- function(platform, version, dates = NULL) {

  # load database
  database <- renv_mran_database_load()

  # get reference to entry in database (initialize if not yet created)
  suffix <- renv_mran_database_key(platform, version)
  database[[suffix]] <- database[[suffix]] %||% new.env(parent = emptyenv())
  entry <- database[[suffix]]

  # rough release dates for R releases
  dates <- as.list(dates %||% renv_mran_database_dates(version))

  for (date in dates) {

    # attempt to update our database entry for this date
    url <- renv_mran_url(date, suffix)
    tryCatch(
      renv_mran_database_update_impl(date, url, entry),
      error = warnify
    )

  }

  # save at end
  printf("[%s]: saving database ... ", date)
  renv_mran_database_save(database)
  writef("DONE")

}

renv_mran_database_update_impl <- function(date, url, entry) {

  printf("[%s]: reading package database ... ", date)

  # get date as number of days since epoch
  idate <- as.integer(date)

  # retrieve available packages
  errors <- new.env(parent = emptyenv())
  db <- renv_available_packages_query_impl(url, errors)
  if (is.null(db)) {
    writef("ERROR")
    return(FALSE)
  }

  # insert packages into database
  for (i in seq_len(nrow(db))) {

    # construct key for index
    name <- db[i, "Package"]
    vers <- db[i, "Version"]
    key <- paste(name, vers)

    # update database
    entry[[key]] <- max(entry[[key]] %||% 0L, idate)

  }

  writef("OK")
  TRUE

}

renv_mran_url <- function(date, suffix) {
  root <- Sys.getenv("RENV_MRAN_URL", unset = "https://mran.microsoft.com/snapshot")
  snapshot <- file.path(root, date)
  paste(snapshot, suffix, sep = "")
}

renv_mran_database_url <- function() {
  default <- "https://rstudio-buildtools.s3.amazonaws.com/renv/mran/packages.rds"
  Sys.getenv("RENV_MRAN_DATABASE_URL", unset = default)
}

renv_mran_database_refresh <- function(explicit = TRUE) {

  if (explicit || renv_mran_database_refresh_required())
    renv_mran_database_refresh_impl()

}

renv_mran_database_refresh_required <- function() {
  dynamic(
    key   = list(),
    value = renv_mran_database_refresh_required_impl()
  )
}

renv_mran_database_refresh_required_impl <- function() {

  # if the cache doesn't exist, we must refresh
  path <- renv_mran_database_path()
  if (!file.exists(path))
    return(TRUE)

  # if we're using an older version of R, but we have newer package
  # versions available in the cache, we don't need to refresh
  db <- tryCatch(renv_mran_database_load(), error = identity)
  if (!inherits(db, "error")) {
    keys <- names(db)
    versions <- unique(basename(keys))
    if (any(versions > getRversion()))
      return(FALSE)
  }

  # read the file mtime
  info <- renv_file_info(path)
  if (is.na(info$mtime))
    return(FALSE)

  # if it's older than a day, then we should update
  difftime(Sys.time(), info$mtime, units = "days") > 1

}

renv_mran_database_refresh_impl <- function() {

  url  <- renv_mran_database_url()
  path <- renv_mran_database_path()

  if (nzchar(url) && nzchar(path)) {
    ensure_parent_directory(path)
    download(url = url, destfile = path, quiet = TRUE)
  }

}

renv_mran_database_sync <- function(platform, version) {

  # read database
  database <- renv_mran_database_load()

  # read entry for this platform + version combo
  key <- renv_mran_database_key(platform, version)
  entry <- database[[key]]
  if (is.null(entry)) {
    database[[key]] <- new.env(parent = emptyenv())
    entry <- database[[key]]
  }

  # get the last known updated date
  last <- max(0L, as.integer(as.list(entry)))
  if (identical(last, 0L)) {
    date <- renv_mran_database_dates(version, all = FALSE)
    last <- as.integer(date)
  }

  # get yesterday's date
  now <- as.integer(as.Date(Sys.time(), tz = "UTC")) - 1L

  # sync up to the last version's release date
  dates <- as.integer(renv_mran_database_dates(version))
  now <- min(now, max(dates))

  # if we've already in sync, nothing to do
  if (last >= now)
    return(FALSE)

  # invoke update for missing dates
  writef("==> Synchronizing MRAN database (%s/%s)", platform, version)
  dates <- as.Date(seq(last + 1L, now, by = 1L), origin = "1970-01-01")
  renv_mran_database_update(platform, version, dates)
  writef("Finished synchronizing MRAN database (%s/%s)", platform, version)

  # return TRUE to indicate update occurred
  return(TRUE)

}

renv_mran_database_sync_all <- function() {

  # NOTE: this needs to be manually updated since the binary URL for
  # packages can change from version to version, especially on macOS

  # R 3.2
  renv_mran_database_sync("windows", "3.2")
  renv_mran_database_sync("macosx/mavericks", "3.2")

  # R 3.3
  renv_mran_database_sync("windows", "3.3")
  renv_mran_database_sync("macosx/mavericks", "3.3")

  # R 3.4
  renv_mran_database_sync("windows", "3.4")
  renv_mran_database_sync("macosx/el-capitan", "3.4")

  # R 3.5
  renv_mran_database_sync("windows", "3.5")
  renv_mran_database_sync("macosx/el-capitan", "3.5")

  # R 3.6
  renv_mran_database_sync("windows", "3.6")
  renv_mran_database_sync("macosx/el-capitan", "3.6")

  # R 4.0
  renv_mran_database_sync("windows", "4.0")
  renv_mran_database_sync("macosx", "4.0")

  # R 4.1
  renv_mran_database_sync("windows", "4.1")
  renv_mran_database_sync("macosx", "4.1")
  renv_mran_database_sync("macosx/big-sur-arm64", "4.1")



}
