
# whether or not the user has enabled the renv watchdog in this session
`_renv_watchdog_enabled` <- NULL

# metadata related to the running watchdog process, if any
`_renv_watchdog_metadata` <- NULL

renv_watchdog_init <- function() {

  `_renv_watchdog_enabled` <<- renv_watchdog_enabled_impl()
  if (!`_renv_watchdog_enabled`)
    return()

  if (!renv_watchdog_running())
    renv_watchdog_start()

}

renv_watchdog_enabled <- function() {
  `_renv_watchdog_enabled`
}

renv_watchdog_enabled_impl <- function() {

  # skip in older versions of R; we require newer APIs
  if (getRversion() < "4.0.0")
    return(FALSE)

  # skip if disabled
  enabled <- Sys.getenv("RENV_WATCHDOG_ENABLED", unset = "TRUE")
  if (!truthy(enabled))
    return(FALSE)

  # skip during R CMD check (but not when running tests)
  checking <- renv_envvar_exists("_R_CHECK_PACKAGE_NAME_")
  if (checking && !is_testing())
    return(FALSE)

  # skip during R CMD build or R CMD INSTALL
  # ... unless we are running tests on CI
  building <-
    renv_envvar_exists("R_PACKAGE_NAME") ||
    renv_envvar_exists("R_PACKAGE_DIR")

  if (building) {
    ci <- Sys.getenv("CI", unset = "FALSE")
    if (!truthy(ci))
      return(FALSE)
  }

  # ok, we're enabled
  TRUE

}

renv_watchdog_start <- function() {

  # create socket for reading initialization data
  repeat {
    port <- sample(49152:65536, size = 1L)
    socket <- tryCatch(serverSocket(port), error = identity)
    if (!inherits(socket, "error"))
      break
  }

  # close socket on exit
  defer(close(socket))

  # set up envvars
  renv_scope_envvars(
    RENV_WATCHDOG_PPID = Sys.getpid(),
    RENV_WATCHDOG_PORT = port
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

  # wait for connection from watchdog server
  conn <- socketAccept(socket, blocking = TRUE)
  defer(close(conn))

  # read initialization data
  `_renv_watchdog_metadata` <<- unserialize(conn)

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
