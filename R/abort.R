
abort <- function(message, ..., class = NULL) {

  # create condition object
  cnd <- if (is.character(message)) {
    data <- list(message = paste(message, collapse = "\n"), ...)
    structure(data, class = c(class, "error", "condition"))
  } else if (inherits(message, "condition")) {
    message
  } else {
    stop("internal error: abort called with unexpected message")
  }

  # signal the condition, giving calling handlers a chance to run first
  signalCondition(cnd)

  # if we got here, then there wasn't any tryCatch() handler on the stack
  # handle printing of the error ourselves, and then stop with fallback
  writeLines(paste("Error:", conditionMessage(cnd)), con = stderr())

  # create the fallback, but 'dodge' the existing error handlers
  fallback <- cnd
  fallback$message <- ""
  class(fallback) <- "condition"

  # disable error printing for the empty error
  renv_scope_options(show.error.messages = FALSE)

  # now throw the error
  stop(fallback)

}
