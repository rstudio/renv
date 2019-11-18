
renv_system_exec <- function(command, args, action, success = 0L) {

  # suppress warnings as some successful commands
  # may return a non-zero exit code, whereas R
  # will always warn on such error codes
  output <- suppressWarnings(
    system2(command, args, stdout = TRUE, stderr = TRUE)
  )

  # extract status code from result
  status <- attr(output, "status") %||% 0L

  # if this status matches an expected 'success' code, return output
  if (status %in% success)
    return(output)

  # otherwise, notify the user that things went wrong
  cmdline <- paste(command, paste(args, collapse = " "))
  underline <- paste(rep.int("=", min(80L, nchar(cmdline))), collapse = "")
  header <- c(cmdline, underline)

  # truncate output (avoid overwhelming console)
  body <- if (length(output) > 200L)
    c(head(output, n = 100L), "< ... >", tail(output, n = 100L))
  else
    output

  # write to stderr
  writeLines(c(header, "", body), con = stderr())

  # throw error
  fmt <- "error %s [error code %i]"
  stopf(fmt, action, status)

}

