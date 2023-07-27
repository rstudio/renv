
renv_warnings_unknown_sources <- function(records) {

  if (empty(records))
    return(FALSE)

  # TODO: Should this be documented?
  enabled <- renv_config_get(
    name    = "unknown.sources",
    scope   = "warnings",
    type    = "logical[1]",
    default = TRUE
  )

  if (!enabled)
    return(FALSE)

  renv_pretty_print_records(
    "The following package(s) were installed from an unknown source:",
    records,
    c(
      "renv may be unable to restore these packages in the future.",
      "Consider reinstalling these packages from a known source (e.g. CRAN)."
    )
  )

  return(TRUE)

}
