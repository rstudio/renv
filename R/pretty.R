
renv_pretty_print <- function(values,
                              preamble  = NULL,
                              postamble = NULL,
                              emitter   = NULL,
                              wrap      = TRUE)
{
  if (renv_tests_running() && !renv_tests_verbose())
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

  emitter <- emitter %||% renv_pretty_print_emitter()
  emitter(text)
}

renv_pretty_print_records <- function(records,
                                      preamble  = NULL,
                                      postamble = NULL,
                                      emitter   = NULL)
{
  if (empty(records))
    return(invisible(NULL))

  if (renv_tests_running() && !renv_tests_verbose())
    return(invisible(NULL))

  packages <- extract_chr(records, "Package")
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

  emitter <- emitter %||% renv_pretty_print_emitter()
  emitter(all)

  invisible(NULL)
}

renv_pretty_print_records_pair <- function(old,
                                           new,
                                           preamble  = NULL,
                                           postamble = NULL,
                                           formatter = NULL,
                                           emitter   = NULL)
{
  formatter <- formatter %||% renv_record_format_pair

  all <- c(
    if (length(preamble)) c(preamble, ""),
    renv_pretty_print_records_pair_impl(old, new, formatter),
    if (length(postamble)) c(postamble, "")
  )

  emitter <- emitter %||% renv_pretty_print_emitter()
  emitter(all)

  invisible(NULL)
}

renv_pretty_print_records_pair_impl <- function(old, new, formatter) {

  renv_scope_locale("LC_COLLATE", "C")
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
  uapply(sort(unique(groups)), function(group) {

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

renv_pretty_print_emitter <- function() {

  emitter <- getOption("renv.pretty.print.emitter", default = NULL)
  if (!is.null(emitter))
    return(emitter)

  if (interactive())
    function(text, ...) writeLines(text, con = stdout(), ...)
  else
    function(text, ...) writeLines(text, con = stderr(), ...)

}
