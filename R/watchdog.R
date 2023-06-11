
# whether or not the user has enabled the renv watchdog in this session
`_renv_watchdog_enabled` <- NULL

# metadata related to the running watchdog process, if any
`_renv_watchdog_process` <- NULL

renv_watchdog_init <- function() {
  `_renv_watchdog_enabled` <<- renv_watchdog_enabled_impl()
}

renv_watchdog_enabled <- function() {
  `_renv_watchdog_enabled`
}

renv_watchdog_check <- function() {

  if (!renv_watchdog_enabled())
    return(FALSE)

  if (renv_watchdog_running())
    return(TRUE)

  renv_watchdog_start()

}

renv_watchdog_enabled_impl <- function() {

  # skip in older versions of R; we require newer APIs
  if (getRversion() < "4.0.0")
    return(FALSE)

  # skip if explicitly disabled via envvar
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

  tryCatch(
    renv_watchdog_start_impl(),
    error = function(e) {
      `_renv_watchdog_enabled` <<- FALSE
      warning(e)
    }
  )

}

renv_watchdog_start_impl <- function() {

  # create a socket server -- this is used so the watchdog process
  # can communicate what port it'll be listening on for messages
  for (i in 1:100) {
    port <- sample(49152:65536, size = 1L)
    socket <- tryCatch(serverSocket(port), error = identity)
    if (!inherits(socket, "error"))
      break
  }

  # check that we got a valid socket
  if (inherits(socket, "error"))
    stop("couldn't create socket server")

  # set up envvars
  renv_scope_envvars(
    RENV_WATCHDOG_PPID = Sys.getpid(),
    RENV_WATCHDOG_PORT = port
  )

  # get path to watchdog script
  script <- system.file("resources/watchdog-process.R", package = "renv")

  # debug logging
  debugging <- Sys.getenv("RENV_WATCHDOG_DEBUG", unset = "TRUE")
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
  conn <- socketAccept(socket, open = "a+b", blocking = TRUE, timeout = 10)
  defer(close(conn))
  `_renv_watchdog_process` <<- unserialize(conn)

  # debugging
  print(`_renv_watchdog_process`)

  TRUE

}

renv_watchdog_notify <- function(method, data) {

  tryCatch(
    renv_watchdog_notify_impl(method, data),
    error = warning
  )

}

renv_watchdog_notify_impl <- function(method, data) {

  # make sure the watchdog is running
  if (!renv_watchdog_check())
    return(FALSE)

  # connect to the running server
  conn <- socketConnection(
    port = renv_watchdog_port(),
    open = "a+b",
    blocking = TRUE
  )

  # close the connection on exit
  defer(close(conn))

  # write message
  message <- list(method = method, data = data)
  serialize(message, connection = conn)

  # TRUE indicates message was written
  TRUE

}

renv_watchdog_port <- function() {
  `_renv_watchdog_process`$port
}

# NOTE: We use 'psnice()' here as R also supports using that
# for process detection on Windows; on all platforms R returns
# NA if you request information about a non-existent process
renv_watchdog_running <- function() {
  pid <- `_renv_watchdog_process`$pid
  !is.null(pid) && !is.na(tools::psnice(pid))
}

renv_watchdog_unload <- function() {
  if (renv_watchdog_running()) {
    pid <- `_renv_watchdog_process`$pid
    tools::pskill(pid)
  }
}
