
renv_report_ok <- function(message, elapsed = NULL) {

  # keep snapshot messages stable
  if (is_testing())
    return(writef("OK [%s in %s]", message, renv_difftime_format_short(elapsed)))

  # treat 'quick' times specially
  if (is.null(elapsed) || elapsed < 0.1)
    return(writef("OK [%s]", message))

  # otherwise, report step with elapsed time
  fmt <- "OK [%s in %s]"
  writef(fmt, message, )

}
