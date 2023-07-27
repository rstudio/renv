
# notice me senpai
notice <- function(preamble, values, postamble = NULL) {

  if (empty(values))
    return()

  lines <- c(
    if (length(preamble)) paste(preamble, collapse = "\n"),
    if (length(values))   paste("-", values, collapse = "\n"),
    if (length(postamble)) paste(postamble, collapse = "\n"),
    ""
  )

  text <- paste(as.character(lines), collapse = "\n")
  renv_notice_impl(text)

}

renv_notice_impl <- function(text) {

  # NOTE: Used by vetiver, so perhaps is part of the API
  # https://github.com/rstudio/renv/issues/1413
  emitter <- getOption("renv.pretty.print.emitter", default = writef)
  emitter(text)

  invisible(NULL)

}
