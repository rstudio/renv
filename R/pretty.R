
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
