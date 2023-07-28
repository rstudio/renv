
caution <- function(fmt = "", ..., con = stdout()) {
  enabled <- getOption("renv.caution.verbose", default = TRUE)
  if (!is.null(fmt) && enabled)
    writeLines(sprintf(fmt, ...), con = con)
}

caution_bullets <- function(preamble = NULL,
                            values = NULL,
                            postamble = NULL,
                            ...,
                            bullets = TRUE,
                            emitter = NULL)
{
  if (empty(values))
    return(invisible())

  renv_dots_check(...)

  lines <- c(
    if (length(preamble))  paste(preamble, collapse = "\n"),
    if (bullets)
      paste("-", values, collapse = "\n")
    else
      paste(values, collapse = "\n"),
    if (length(postamble)) paste(postamble, collapse = "\n"),
    ""
  )

  text <- paste(lines, collapse = "\n")
  renv_caution_impl(text, emitter)
}

renv_caution_impl <- function(text, emitter = NULL) {

  # NOTE: Used by vetiver, so perhaps is part of the API.
  # We should think of a cleaner way of exposing this.
  # https://github.com/rstudio/renv/issues/1413
  emitter <- emitter %||% {
    getOption("renv.pretty.print.emitter", default = caution)
  }

  emitter(text)
  invisible(NULL)

}
