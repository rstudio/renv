
renv_report_user_cancel <- function(reason = NULL) {
  message("* Operation canceled.")
  renv_snapshot_auto_suppress_next()
}
