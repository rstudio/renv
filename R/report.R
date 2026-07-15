
renv_report_ok_message <- function(message, elapsed = 0, verbose = FALSE) {

  # handle verbose messages first
  if (verbose) {
    fmt <- "- OK [%s in %s]"
    return(sprintf(fmt, message, renv_difftime_format_short(elapsed)))
  }

  # treat 'quick' times specially
  if (!testing() && elapsed < 0.5)
    return(sprintf("OK [%s]", message))

  # otherwise, report step with elapsed time
  fmt <- "OK [%s in %s]"
  sprintf(fmt, message, renv_difftime_format_short(elapsed))

}
