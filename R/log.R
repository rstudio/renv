
log <- function(level, scope, fmt, ...) {
  if (level >= getOption("renv.log.level", default = 4L))
    renv_log_impl(scope, fmt, ...)
}

elog <- function(scope, fmt, ...) {
  log(4L, scope, fmt, ...)
}

wlog <- function(scope, fmt, ...) {
  log(3L, scope, fmt, ...)
}

ilog <- function(scope, fmt, ...) {
  log(2L, scope, fmt, ...)
}

dlog <- function(scope, fmt, ...) {
  log(1L, scope, fmt, ...)
}


renv_log_impl <- function(scope, fmt, ...) {

  # build message
  message <- sprintf(fmt, ...)

  # annotate message
  fmt <- "[%s] [renv-%s]: %s"
  now <- format(Sys.time(), tz = "UTC")
  all <- sprintf(fmt, now, scope, message)

  # write it out
  file <- getOption("renv.log.file", default = stdout())
  cat(all, file = file, sep = "\n", append = TRUE)

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
    level %in% c("4", "error",   "ERROR")   ~ 4L,
    level %in% c("3", "warning", "WARNING") ~ 3L,
    level %in% c("2", "info",    "INFO")    ~ 2L,
    level %in% c("1", "debug",   "DEBUG")   ~ 1L,
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
