
renv_dots_disallow <- function(...) {

  dots <- list(...)
  if (length(dots) == 0)
    return(TRUE)

  call <- sys.call(sys.parent())
  func <- sys.function(sys.parent())
  matched <- match.call(func, call, expand.dots = FALSE)

  dotcall <- format(matched["..."])
  start <- regexpr("(", dotcall, fixed = TRUE)
  end <- nchar(dotcall) - 2L
  args <- substring(dotcall, start, end)

  message <- paste(
    "unused",
    plural("argument", length(matched[["..."]])),
    args
  )

  err <- simpleError(message = message, call = call)
  stop(err)

}
