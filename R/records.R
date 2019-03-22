
# tools for interacting with the R package records encoded
# within a lockfile
renv_records <- function(records) {
  records$R$Package %NULL% records
}

renv_records_select <- function(records, actions, action) {
  records <- renv_records(records)
  records[names(actions[actions == action])]
}

renv_records_sort <- function(records) {
  renv_scope_locale("LC_COLLATE", "C")
  records[order(names(records))]
}
