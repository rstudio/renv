
# notice me senpai
notice <- function(preamble,
                   values,
                   postamble = NULL,
                   emitter = NULL)
{
  if (empty(values))
    return()

  lines <- c(
    if (length(preamble)) paste(preamble, collapse = "\n"),
    if (length(values))   paste("-", values, collapse = "\n"),
    if (length(postamble)) paste(postamble, collapse = "\n"),
    ""
  )

  text <- paste(as.character(lines), collapse = "\n")
  renv_notice_impl(text, emitter)
}

renv_notice_impl <- function(text, emitter = NULL) {

  # NOTE: Used by vetiver, so perhaps is part of the API.
  # We should think of a cleaner way of exposing this.
  # https://github.com/rstudio/renv/issues/1413
  emitter <- emitter %||% {
    getOption("renv.pretty.print.emitter", default = caution)
  }

  emitter(text)
  invisible(NULL)

}
