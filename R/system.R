
renv_system_exec <- function(command, args, action, success = 0L) {

  output <- suppressWarnings(
    system2(command, args, stdout = TRUE, stderr = TRUE)
  )

  status <- attr(output, "status") %||% 0L
  if (!status %in% success) {
    writeLines(output, con = stderr())
    fmt <- "error %s [error code %i]"
    stopf(fmt, action, status)
  }

  output

}

