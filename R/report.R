
renv_report_cancelled <- function(reason = NULL) {
  message("* Operation canceled.")
  renv_snapshot_auto_suppress_next()
}
