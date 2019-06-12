
renv_pretty_print <- function(values,
                              preamble = NULL,
                              postamble = NULL,
                              emitter = NULL,
                              wrap = TRUE)
{
  msg <- stack()

  if (!is.null(preamble)) {
    msg$push(paste(preamble, collapse = "\n"))
    msg$push("")
  }

  formatted <- if (wrap)
    strwrap(paste(values, collapse = ", "), width = 60)
  else
    values

  msg$push(paste("\t", formatted, sep = "", collapse = "\n"))

  if (!is.null(postamble)) {
    msg$push("")
    msg$push(paste(postamble, collapse = "\n"))
  }

  msg$push("")
  text <- paste(as.character(msg$data()), collapse = "\n")

  emitter <- emitter %||% writeLines
  emitter(text)
}

renv_pretty_print_records <- function(records,
                                      preamble = NULL,
                                      postamble = NULL)
{
  formatted <- named(
    sprintf("  [%s]", map_chr(extract(records, "Version"), format)),
    sprintf("  %s",   map_chr(extract(records, "Package"), format))
  )

  preamble %&&% writeLines(preamble)
  print.simple.list(formatted)
  writeLines("")
  postamble %&&% writeLines(postamble)
  postamble %&&% writeLines("")

  invisible(NULL)
}

renv_pretty_print_records_pair <- function(before,
                                           after,
                                           preamble = NULL,
                                           postamble = NULL)
{
  if (!setequal(names(before), names(after)))
    stopf("internal error: names mismatch", call. = TRUE)

  nm <- intersect(names(before), names(after))
  before <- before[nm]; after <- after[nm]

  formatted <- sprintf(
    "  [%s -> %s]",
    map_chr(extract(before, "Version"), format),
    map_chr(extract(after, "Version"), format)
  )

  names(formatted) <- sprintf("  %s", extract_chr(before, "Package"))

  preamble %&&% writeLines(preamble)
  print.simple.list(formatted)
  writeLines("")
  postamble %&&% writeLines(postamble)
  postamble %&&% writeLines("")

  invisible(NULL)
}
