
# NOTE: We use 'psnice()' here as R also supports using that
# for process detection on Windows; on all platforms R returns
# NA if you request information about a non-existent process
renv_process_exists <- function(pid) {
  !is.na(psnice(pid))
}

renv_process_kill <- function(pid, signal = 15L) {
  pskill(pid, signal)
}
