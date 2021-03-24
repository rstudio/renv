
renv_report_user_cancel <- function() {
  message("* Operation aborted.")
  renv_snapshot_auto_suppress_next()
}
