
renv_pretty_print <- function(preamble, values, postamble = NULL) {

  if (!renv_verbose() || empty(values))
    return()

  msg <- stack()
  msg$push(paste(preamble, collapse = "\n"))
  msg$push("")

  msg$push(paste0("- ", values, collapse = "\n"))

  if (!is.null(postamble)) {
    msg$push("")
    msg$push(paste(postamble, collapse = "\n"))
  }

  msg$push("")

  text <- paste(as.character(msg$data()), collapse = "\n")
  renv_pretty_print_impl(text)

}

renv_pretty_print_impl <- function(text) {

  # NOTE: Used by vetiver, so perhaps is part of the API
  # https://github.com/rstudio/renv/issues/1413
  emitter <- getOption("renv.pretty.print.emitter", default = writef)
  emitter(text)

  invisible(NULL)

}

renv_pretty_print_records <- function(preamble, records, postamble = NULL)
{
  if (empty(records))
    return(invisible(NULL))

  if (!renv_verbose())
    return(invisible(NULL))

  # NOTE: use 'sort()' rather than 'csort()' here so that
  # printed output is sorted in the expected way in the users locale
  # https://github.com/rstudio/renv/issues/1289
  names(records) <- names(records) %||% map_chr(records, `[[`, "Package")
  records <- records[sort(names(records))]
  packages <- names(records)
  descs <- map_chr(records, renv_record_format_short)

  text <- sprintf("- %s [%s]", format(packages), descs)

  all <- c(
    preamble, "",
    text, "",
    postamble, if (length(postamble)) ""
  )

  renv_pretty_print_impl(all)
}

renv_pretty_print_records_pair <- function(preamble,
                                           old,
                                           new,
                                           postamble = NULL,
                                           formatter = NULL)
{
  formatter <- formatter %||% renv_record_format_pair

  all <- c(
    c(preamble, ""),
    renv_pretty_print_records_pair_impl(old, new, formatter),
    if (length(postamble)) c(postamble, "")
  )

  renv_pretty_print_impl(all)
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
