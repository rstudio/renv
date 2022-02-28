
log <- function(level, fmt, ...) {

  if (level >= getOption("renv.log.level", default = 4L)) {
    msg <- sprintf(fmt, ...)
    renv_log_impl(msg)
  }

}

elog <- function(fmt, ...) {
  log(4L, fmt, ...)
}

wlog <- function(fmt, ...) {
  log(3L, fmt, ...)
}

ilog <- function(fmt, ...) {
  log(2L, fmt, ...)
}

dlog <- function(fmt, ...) {
  log(1L, fmt, ...)
}


renv_log_impl <- function(msg) {

  # build log message
  fmt <- "[%s] renv: %s"
  now <- format(Sys.time(), tz = "UTC")
  all <- sprintf(fmt, now, msg)

  # write it out
  con <- getOption("renv.log.file", default = stdout())
  cat(all, file = con, sep = "\n", append = TRUE)

}

renv_log_init <- function() {
  renv_log_init_level()
  renv_log_init_file()
}

renv_log_init_level <- function() {

  # check for environment variable
  level <- Sys.getenv("RENV_LOG_LEVEL", unset = NA)
  if (is.na(level))
    return()

  # read and assign
  override <- case(
    level %in% c("error",   "ERROR")   ~ 4L,
    level %in% c("warning", "WARNING") ~ 3L,
    level %in% c("info",    "INFO")    ~ 2L,
    level %in% c("debug",   "DEBUG")   ~ 1L,
    ~ warningf("ignoring invalid RENV_LOG_LEVEL environment variable")
  )

  options(renv.log.level = override)

}

renv_log_init_file <- function() {

  file <- Sys.getenv("RENV_LOG_FILE", unset = NA)
  if (is.na(file))
    return()

  options(renv.log.file = file)

}
