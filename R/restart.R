
renv_request_restart <- function(reason, ...) {

  # use 'restart' helper defined by front-end (if any)
  restart <- getOption("restart")
  if (is.function(restart))
    return(invisible(restart()))

  # otherwise, ask the user to restart
  fmt <- "* %s -- please restart the R session."
  messagef(fmt, sprintf(reason, ...))

  invisible()
}
