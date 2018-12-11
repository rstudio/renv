
renv_pretty_print <- function(message, manifest, actions, action) {

  matches <- actions[actions == action]
  if (empty(matches))
    return()

  entries <- manifest$R$Packages[names(matches)]
  formatted <- named(
    sprintf("  [%s]", map_chr(extract(entries, "Version"), format)),
    sprintf("  %s",   map_chr(extract(entries, "Package"), format))
  )

  writeLines(message)
  print.simple.list(formatted)
  writeLines("")

}

renv_pretty_print_pair <- function(message, old, new, actions, action) {

  matches <- actions[actions == action]
  if (empty(matches))
    return()

  before <- old$R$Packages[names(matches)]
  after  <- new$R$Packages[names(matches)]

  formatted <- sprintf(
    "[%s -> %s]",
    map_chr(extract(before, "Version"), format),
    map_chr(extract(after, "Version"), format)
  )

  names(formatted) <- sprintf("  %s", extract_chr(before, "Package"))

  writeLines(message)
  print.simple.list(formatted)
  writeLines("")

}
