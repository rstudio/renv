
# whether or not the user has enabled the renv watchdog in this session
`_renv_watchdog_enabled` <- NULL

# metadata related to the running watchdog process, if any
`_renv_watchdog_process` <- NULL

renv_watchdog_init <- function() {

  `_renv_watchdog_enabled` <<- renv_watchdog_enabled_impl()

  reg.finalizer(renv_envir_self(), function(envir) {

    # nothing to do if watchdog isn't running
    if (!renv_watchdog_running())
      return()

    # tell watchdog to shutdown
    renv_watchdog_notify("Shutdown")
    renv_watchdog_unload()

    # wait for process to exit (avoid RStudio bomb)
    for (i in 1:10) {
      if (renv_watchdog_running()) {
        Sys.sleep(0.1)
      } else {
        break
      }
    }

  }, onexit = TRUE)

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
  server <- renv_socket_server()
  socket <- server$socket; port <- server$port
  defer(close(socket))

  # generate script to invoke watchdog
  script <- renv_scope_tempfile("renv-watchdog-", fileext = ".R")
  code <- substitute({
    client <- list(pid = pid, port = port)
    asNamespace("renv")$renv_watchdog_server_start(client)
  }, list(pid = Sys.getpid(), port = port))
  writeLines(deparse(code), con = script)

  # debug logging
  debugging <- Sys.getenv("RENV_WATCHDOG_DEBUG", unset = "FALSE")
  stdout <- stderr <- if (truthy(debugging)) "" else FALSE

  # launch the watchdog
  # TODO: How do we make this work when running tests?
  system2(
    command = R(),
    args = c("--vanilla", "-s", "-f", renv_shell_path(script)),
    stdout = stdout,
    stderr = stderr,
    wait = FALSE
  )

  # wait for connection from watchdog server
  conn <- renv_socket_accept(socket, open = "rb", timeout = 10)
  defer(close(conn))

  # store information about the running process
  `_renv_watchdog_process` <<- unserialize(conn)

  # return TRUE to indicate process was started
  TRUE

}

renv_watchdog_notify <- function(method, data = list()) {

  tryCatch(
    renv_watchdog_notify_impl(method, data),
    error = warning
  )

}

renv_watchdog_notify_impl <- function(method, data = list()) {

  # make sure the watchdog is running
  if (!renv_watchdog_check())
    return(FALSE)

  # connect to the running server
  port <- renv_watchdog_port()
  conn <- renv_socket_connect(port, open = "wb")

  # close the connection on exit
  defer(close(conn))

  # write message
  message <- list(method = method, data = data)
  serialize(message, connection = conn)

  # TRUE indicates message was written
  TRUE

}

renv_watchdog_request <- function(method, data = list()) {
  tryCatch(
    renv_watchdog_request_impl(method, data),
    error = warning
  )
}

renv_watchdog_request_impl <- function(method, data = list()) {

  # make sure the watchdog is running
  if (!renv_watchdog_check())
    return(FALSE)

  # connect to the running server
  port <- renv_watchdog_port()
  outgoing <- renv_socket_connect(port, open = "wb")
  defer(close(outgoing))

  # create our own socket server
  server <- renv_socket_server()
  defer(close(server$socket))

  # write message
  message <- list(method = method, data = data, port = server$port)
  serialize(message, connection = outgoing)

  # now, open a new connection to get the response
  incoming <- renv_socket_accept(server$socket, open = "rb")
  defer(close(incoming))

  # read the response
  unserialize(connection = incoming)

}

renv_watchdog_pid <- function() {
  `_renv_watchdog_process`$pid
}

renv_watchdog_port <- function() {
  `_renv_watchdog_process`$port
}

renv_watchdog_running <- function() {
  pid <- renv_watchdog_pid()
  !is.null(pid) && renv_process_exists(pid)
}

renv_watchdog_unload <- function() {
  if (renv_watchdog_running()) {
    pid <- renv_watchdog_pid()
    renv_process_kill(pid)
  }
}
