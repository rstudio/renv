
renv_dots_check <- function(...) {

  dots <- list(...)
  parent <- parent.frame()

  # accept 'bioc' as an alias for 'bioconductor'
  bioc <- dots[["bioc"]]
  if (!is.null("bioc") && exists("bioconductor", envir = parent)) {
    assign("bioconductor", bioc, envir = parent)
    dots[["bioc"]] <- NULL
  }

  # allow 'confirm' as an alias for 'prompt'
  confirm <- dots[["confirm"]]
  if (!is.null(confirm) && exists("prompt", envir = parent)) {
    assign("prompt", confirm, envir = parent)
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
  n <- length(matched[["..."]])

  message <- paste("unused", plural("argument", n), args)
  err <- simpleError(message = message, call = call)
  stop(err)

}
