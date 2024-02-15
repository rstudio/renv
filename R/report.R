
renv_report_ok <- function(message, elapsed = 0, verbose = FALSE) {

  # handle verbose printing first
  if (verbose) {
    fmt <- "- OK [%s in %s]"
    return(writef(fmt, message, renv_difftime_format_short(elapsed)))
  }

  # treat 'quick' times specially
  if (!testing() && elapsed < 0.1)
    return(writef("OK [%s]", message))

  # otherwise, report step with elapsed time
  fmt <- "OK [%s in %s]"
  writef(fmt, message, renv_difftime_format_short(elapsed))

}
