
renv_pretty_print <- function(message, lockfile, actions, action) {

  matches <- actions[actions == action]
  if (empty(matches))
    return()

  entries <- lockfile$R$Package[names(matches)]
  formatted <- named(
    sprintf("  [%s]", map_chr(extract(entries, "Version"), format)),
    sprintf("  %s",   map_chr(extract(entries, "Package"), format))
  )

  writeLines(message)
  print.simple.list(formatted)
  writeLines("")

}

renv_pretty_print_pair <- function(message, old, new, actions, action) {

  matches <- actions[actions %in% action]
  if (empty(matches))
    return()

  before <- old$R$Package[names(matches)]
  after  <- new$R$Package[names(matches)]

  formatted <- sprintf(
    "  [%s -> %s]",
    map_chr(extract(before, "Version"), format),
    map_chr(extract(after, "Version"), format)
  )

  names(formatted) <- sprintf("  %s", extract_chr(before, "Package"))

  writeLines(message)
  print.simple.list(formatted)
  writeLines("")

}

renv_pretty_print_packages <- function(packages,
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
    strwrap(paste(packages, collapse = ", "), width = 60)
  else
    packages

  msg$push(paste("\t", formatted, sep = "", collapse = "\n"))

  if (!is.null(postamble)) {
    msg$push("")
    msg$push(paste(postamble, collapse = "\n"))
  }

  text <- paste(as.character(msg$data()), collapse = "\n")

  emitter <- emitter %||% writeLines
  emitter(text)
}
