
abort <- function(message, ..., body = NULL, class = NULL) {

  renv_dots_check(...)

  # create condition object
  cnd <- if (is.character(message)) {
    structure(class = c(class, "error", "condition"), list(
      message = paste(c(message, body), collapse = "\n"),
      meta = list(message = message, body = body),
      ...
    ))
  } else if (inherits(message, "condition")) {
    message
  } else {
    stop("internal error: abort called with unexpected message")
  }

  # if we were called with a custom condition object not having our meta,
  # just throw it as-is
  if (is.null(cnd$meta))
    stop(cnd)

  # signal the condition, giving calling handlers a chance to run first
  signalCondition(cnd)

  # if we got here, then there wasn't any tryCatch() handler on the stack.
  # handle printing of the error ourselves, and then stop with fallback.
  parts <- c(cnd$meta$body, "", paste("Error:", cnd$meta$message))
  writeLines(paste(parts, collapse = "\n"), con = stderr())

  # create the fallback, but 'dodge' the existing error handlers
  fallback <- cnd
  fallback$message <- ""
  class(fallback) <- "condition"

  # disable error printing for the empty error
  renv_scope_options(show.error.messages = FALSE)

  # now throw the error
  stop(fallback)

}
