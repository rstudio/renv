
renv_system_exec <- function(command,
                             args    = NULL,
                             action  = "executing command",
                             success = 0L,
                             stream  = FALSE,
                             quiet   = NULL)
{
  # be quiet when running tests by default
  quiet <- quiet %||% renv_tests_running()

  # handle 'stream' specially
  if (stream) {

    # form stdout, stderr
    stdout <- stderr <- if (quiet) FALSE else ""

    # execute command
    status <- suppressWarnings(
      if (is.null(args))
        system(command, ignore.stdout = quiet, ignore.stderr = quiet)
      else
        system2(command, args, stdout = stdout, stderr = stderr)
    )

    # check for error
    status <- status %||% 0L
    if (!is.null(success) && !status %in% success) {
      fmt <- "error %s [error code %i]"
      stopf(fmt, action, status)
    }

    # return status code
    return(status)

  }

  # suppress warnings as some successful commands may return a non-zero exit
  # code, whereas R will always warn on such error codes
  output <- suppressWarnings(
    if (is.null(args))
      system(command, intern = TRUE)
    else
      system2(command, args, stdout = TRUE, stderr = TRUE)
  )

  # extract status code from result
  status <- attr(output, "status") %||% 0L

  # if this status matches an expected 'success' code, return output
  if (is.null(success) || status %in% success)
    return(output)

  # otherwise, notify the user that things went wrong
  if (!quiet) {

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

  }

  # throw error
  fmt <- "error %s [error code %i]"
  stopf(fmt, action, status)
}

