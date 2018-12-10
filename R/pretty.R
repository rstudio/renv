
renv_pretty_print <- function(message, manifest, actions, action) {

  matches <- actions[actions == action]
  if (empty(matches))
    return()

  entries <- manifest$library[names(matches)]
  formatted <- named(
    sprintf("  [%s]", extract(entries, "Version")),
    sprintf("  %s",   extract(entries, "Package"))
  )

  writeLines(message)
  print.simple.list(formatted)
  writeLines("")

}

renv_pretty_print_pair <- function(message, old, new, actions, action) {

  matches <- actions[actions == action]
  if (empty(matches))
    return()

  before <- old$library[names(matches)]
  after  <- new$library[names(matches)]

  formatted <- sprintf(
    "[%s -> %s]",
    extract(before, "Version"),
    extract(after, "Version")
  )

  names(formatted) <- sprintf("  %s", extract_chr(before, "Package"))

  writeLines(message)
  print.simple.list(formatted)
  writeLines("")

}
