
# the log level, indicating what severity of messages will be logged
the$log_level <- 4L

# the file to which log messages will be written
the$log_file <- NULL

# the scopes for which filtering will be enabled
the$log_scopes <- NULL

elog <- function(scope, fmt, ...) {
  renv_log_impl(4L, scope, fmt, ...)
}

wlog <- function(scope, fmt, ...) {
  renv_log_impl(3L, scope, fmt, ...)
}

ilog <- function(scope, fmt, ...) {
  renv_log_impl(2L, scope, fmt, ...)
}

dlog <- function(scope, fmt, ...) {
  renv_log_impl(1L, scope, fmt, ...)
}


renv_log_impl <- function(level, scope, fmt, ...) {

  # check log level
  if (level < the$log_level)
    return()

  # only include scopes matching the scopes
  scopes <- the$log_scopes
  if (is.character(scopes) && !scope %in% scopes)
    return()

  # build message
  message <- sprintf(fmt, ...)

  # annotate with prefix from scope, timestamp
  fmt <- "%sZ [renv-%i] %s: %s"
  now <- format(Sys.time(), format = "%Y-%m-%d %H:%M:%OS6", tz = "UTC")
  all <- sprintf(fmt, now, Sys.getpid(), scope, message)

  # write it out
  cat(all, file = the$log_file, sep = "\n", append = TRUE)

}

renv_log_init <- function(level = NULL, file = NULL, scopes = NULL) {
  the$log_level  <- renv_log_level(level)
  the$log_file   <- renv_log_file(file)
  the$log_scopes <- renv_log_scopes(scopes)
}

renv_log_level <- function(level = NULL) {

  level <- level %||% Sys.getenv("RENV_LOG_LEVEL", unset = NA)
  if (is.na(level))
    return(4L)

  case(
    level %in% c("4", "error",   "ERROR")   ~ 4L,
    level %in% c("3", "warning", "WARNING") ~ 3L,
    level %in% c("2", "info",    "INFO")    ~ 2L,
    level %in% c("1", "debug",   "DEBUG")   ~ 1L,
    ~ {
      warningf("ignoring invalid RENV_LOG_LEVEL '%s'", level)
      4L
    }
  )

}

renv_log_file <- function(file = NULL) {

  # check for log file
  file <- file %||% Sys.getenv("RENV_LOG_FILE", unset = NA)
  if (!is.na(file))
    return(file)

  # default to stderr, since it's unbuffered
  stderr()

}

renv_log_scopes <- function(scopes = NULL) {

  scopes <- scopes %||% Sys.getenv("RENV_LOG_SCOPES", unset = NA)
  if (is.na(scopes))
    return(NULL)

  strsplit(scopes, ",", fixed = TRUE)[[1L]]

}

