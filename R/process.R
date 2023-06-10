
renv_process_exists <- function(pid) {
  !is.na(tools::psnice(pid))
}
