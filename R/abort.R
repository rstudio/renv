
abort <- function(message, ..., details = NULL, class = NULL) {

  # create condition object
  cnd <- if (is.character(message)) {
    data <- list(message = message, details = details, ...)
    structure(data, class = c(class, "error", "condition"))
  } else if (inherits(message, "condition")) {
    message
  } else {
    stop("internal error: abort called with unexpected message")
  }

  # signal the condition, giving calling handlers a chance to run first
  signalCondition(cnd)

  # if we get here, the condition wasn't handled -- handle printing of
  # the error ourselves, and then stop with a fallback condition
  if (length(cnd$details))
    writeLines(c(cnd$details, ""))

  # create the fallback, but 'dodge' the existing error handlers
  fallback <- cnd
  class(fallback) <- "condition"

  # now throw the error
  stop(fallback)

}
