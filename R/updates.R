
renv_updates_create <- function(diff, old, new) {
  structure(
    list(diff = diff, old = old, new = new),
    class = "renv_updates"
  )
}

renv_updates_report <- function(diff, old, new) {

  lhs <- renv_records(old)
  rhs <- renv_records(new)
  renv_pretty_print_records_pair(
    lhs[names(lhs) %in% names(diff)],
    rhs[names(rhs) %in% names(diff)]
  )

}
