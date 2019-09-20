
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

renv_pretty_print_records_pair <- function(old,
                                           new,
                                           preamble = NULL,
                                           postamble = NULL)
{
  preamble %&&% writeLines(c(preamble, ""))
  renv_pretty_print_records_impl(old, new)
  postamble %&&% writeLines(c(postamble, ""))
  invisible(NULL)
}

renv_pretty_print_records_impl <- function(old, new) {

  all <- union(names(old), names(new))

  # compute groups
  groups <- map_chr(all, function(package) {

    lhs <- old[[package]]; rhs <- new[[package]]
    case(
      is.null(lhs$Source)      ~ rhs$Repository %||% rhs$Source,
      is.null(rhs$Source)      ~ lhs$Repository %||% lhs$Source,
      !is.null(rhs$Repository) ~ rhs$Repository,
      !is.null(rhs$Source)     ~ rhs$Source
    )

  })

  n <- max(nchar(all))

  # iterate over each group and print
  lapply(sort(unique(groups)), function(group) {

    lhs <- collapse(renv_records_select(old, groups, group))
    rhs <- collapse(renv_records_select(new, groups, group))

    nms <- union(names(lhs), names(rhs))
    text <- map_chr(nms, function(nm) {
      renv_record_format_pair(lhs[[nm]], rhs[[nm]])
    })

    if (group == "unknown")
      group <- "(Unknown Source)"

    vwritef(header(group))
    vwritef(paste("-", format(nms, width = n), " ", text))
    vwritef("")

  })

}
