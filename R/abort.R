
abort <- function(message, ..., body = NULL, class = NULL) {

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
  all <- c(
    cnd$meta$body, if (length(cnd$meta$body)) "",
    paste("Error:", paste(cnd$meta$message, collapse = "\n"))
  )

  # write error message to stderr, as errors might normally do
  writeLines(all, con = stderr())

  # create the fallback, but 'dodge' the existing error handlers
  fallback <- cnd
  fallback$message <- ""
  class(fallback) <- "condition"

  # disable error printing for the empty error
  renv_scope_options(show.error.messages = FALSE)

  # now throw the error
  stop(fallback)

}
