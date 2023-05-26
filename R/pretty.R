
renv_pretty_print <- function(values,
                              preamble  = NULL,
                              postamble = NULL,
                              emitter   = NULL,
                              wrap      = TRUE)
{
  if (!renv_verbose())
    return()

  msg <- stack()
  if (empty(values))
    return()

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

  writef(text)
}

renv_pretty_print_records <- function(records,
                                      preamble  = NULL,
                                      postamble = NULL)
{
  if (empty(records))
    return(invisible(NULL))

  if (!renv_verbose())
    return(invisible(NULL))

  names(records) <- names(records) %||% map_chr(records, `[[`, "Package")
  records <- records[csort(names(records))]
  packages <- names(records)
  descs <- map_chr(records, renv_record_format_short)

  lhs <- paste(" ", format(packages))
  rhs <- descs

  n <- max(nchar(lhs))
  header <- paste(c(rep.int(" ", n + 1), "_"), collapse = "")
  text <- sprintf("%s   [%s]", lhs, rhs)

  all <- c(
    if (length(preamble)) preamble,
    c(header, text, ""),
    if (length(postamble)) c(postamble, "")
  )

  writef(all)

  invisible(NULL)
}

renv_pretty_print_records_pair <- function(old,
                                           new,
                                           preamble  = NULL,
                                           postamble = NULL,
                                           formatter = NULL)
{
  formatter <- formatter %||% renv_record_format_pair

  all <- c(
    if (length(preamble)) c(preamble, ""),
    renv_pretty_print_records_pair_impl(old, new, formatter),
    if (length(postamble)) c(postamble, "")
  )

  writef(all)

  invisible(NULL)
}

renv_pretty_print_records_pair_impl <- function(old, new, formatter) {

  # NOTE: use 'sort()' rather than 'csort()' here so that
  # printed output is sorted in the expected way in the users locale
  # https://github.com/rstudio/renv/issues/1289
  all <- sort(union(names(old), names(new)))

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
  uapply(csort(unique(groups)), function(group) {

    lhs <- renv_records_select(old, groups, group)
    rhs <- renv_records_select(new, groups, group)

    nms <- union(names(lhs), names(rhs))
    text <- map_chr(nms, function(nm) {
      formatter(lhs[[nm]], rhs[[nm]])
    })

    if (group == "unknown")
      group <- "(Unknown Source)"

    c(
      header(group),
      paste("-", format(nms, width = n), " ", text),
      ""
    )

  })

}
