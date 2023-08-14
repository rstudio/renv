
renv_report_ok <- function(message, elapsed = 0) {

  # treat 'quick' times specially
  if (!renv_package_testing() && elapsed < 0.1)
    return(writef("OK [%s]", message))

  # otherwise, report step with elapsed time
  fmt <- "OK [%s in %s]"
  writef(fmt, message, renv_difftime_format_short(elapsed))

}
