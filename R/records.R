
# tools for interacting with the R package records encoded
# within a lockfile
renv_records <- function(records) {
  records$R$Package %||% records
}

renv_records_select <- function(records, actions, action) {
  records <- renv_records(records)
  records[names(actions[actions == action])]
}
