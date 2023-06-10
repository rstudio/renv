
`_renv_watchdog_metadata` <- NULL

renv_watchdog_init <- function() {
  if (!renv_watchdog_running())
    renv_watchdog_start()
}

renv_watchdog_start <- function() {

  # skip if disabled
  enabled <- Sys.getenv("RENV_WATCHDOG_ENABLED", unset = "TRUE")
  if (!truthy(enabled))
    return()

  # skip during R CMD build or R CMD INSTALL
  building <-
    renv_envvar_exists("R_PACKAGE_NAME") ||
    renv_envvar_exists("R_PACKAGE_DIR")

  if (building)
    return()

  # set up path for initialization data
  pattern <- sprintf("renv-watchdog-%i-", Sys.getpid())
  init <- renv_scope_tempfile(pattern, fileext = ".rds")

  # set up envvars
  renv_scope_envvars(
    RENV_WATCHDOG_PPID = Sys.getpid(),
    RENV_WATCHDOG_INIT = init
  )

  # get path to watchdog script
  script <- system.file("resources/watchdog-server.R", package = "renv")

  # debug logging
  debugging <- Sys.getenv("RENV_WATCHDOG_DEBUG", unset = "FALSE")
  stdout <- stderr <- if (truthy(debugging)) "" else FALSE

  # launch the watchdog
  system2(
    command = R(),
    args = c("--vanilla", "-s", "-f", renv_shell_path(script)),
    stdout = stdout,
    stderr = stderr,
    wait = FALSE
  )

  # wait for metadata
  renv_file_wait(init, timeout = 10L)
  `_renv_watchdog_metadata` <<- readRDS(init)

}

renv_watchdog_notify <- function(method, data) {

  # establish connection
  port <- renv_watchdog_port()
  conn <- socketConnection(port = renv_watchdog_port())
  on.exit(close(conn), add = TRUE)

  # write data
  serialize(
    list(method = method, data = data),
    connection = conn
  )

}

renv_watchdog_port <- function() {
  `_renv_watchdog_metadata`$port
}

renv_watchdog_running <- function(pid = NULL) {
  pid <- pid %||% `_renv_watchdog_metadata`$pid
  !is.null(pid) && !is.na(tools::psnice(pid))
}

renv_watchdog_unload <- function() {
  pid <- `_renv_watchdog_metadata`$pid
  if (!is.null(pid))
    tools::pskill(pid)
}
