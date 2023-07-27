
renv_updates_create <- function(diff, old, new) {
  structure(
    list(diff = diff, old = old, new = new),
    class = "renv_updates"
  )
}

renv_updates_report <- function(preamble, diff, old, new) {

  lhs <- renv_lockfile_records(old)
  rhs <- renv_lockfile_records(new)
  renv_pretty_print_records_pair(
    preamble,
    lhs[names(lhs) %in% names(diff)],
    rhs[names(rhs) %in% names(diff)]
  )

}
