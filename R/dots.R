
renv_dots_check <- function(...) {

  dots <- list(...)

  # allow 'confirm' as an alias for 'prompt'
  confirm <- dots[["confirm"]]
  if (!is.null(confirm) && exists("prompt", envir = parent.frame())) {
    assign("prompt", confirm, envir = parent.frame())
    dots[["confirm"]] <- NULL
  }

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
