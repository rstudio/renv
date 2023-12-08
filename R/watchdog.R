
# whether or not the user has enabled the renv watchdog in this session
the$watchdog_enabled <- FALSE

# metadata related to the running watchdog process, if any
the$watchdog_process <- NULL

renv_watchdog_init <- function() {
  the$watchdog_enabled <- renv_watchdog_enabled_impl()
}

renv_watchdog_enabled <- function() {
  the$watchdog_enabled
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

  # allow override via environment variable
  enabled <- Sys.getenv("RENV_WATCHDOG_ENABLED", unset = NA)
  if (!is.na(enabled))
    return(truthy(enabled))

  # disable on Windows; need to understand CI test failures
  # https://github.com/rstudio/renv/actions/runs/5273668333/jobs/9537353788#step:6:242
  if (renv_platform_windows())
    return(FALSE)

  # skip during R CMD check (but not when running tests)
  checking <- renv_envvar_exists("_R_CHECK_PACKAGE_NAME_")
  if (checking && !testing())
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

  the$watchdog_enabled <- tryCatch(
    renv_watchdog_start_impl(),
    error = function(e) {
      warning(conditionMessage(e))
      FALSE
    }
  )

}

renv_watchdog_start_impl <- function() {

  # create a socket server -- this is used so the watchdog process
  # can communicate what port it'll be listening on for messages
  dlog("watchdog", "launching watchdog")
  server <- renv_socket_server()
  socket <- server$socket; port <- server$port
  defer(close(socket))

  # generate script to invoke watchdog
  script <- renv_scope_tempfile("renv-watchdog-", fileext = ".R")

  # figure out library path -- need to dodge devtools::load_all()
  nspath <- renv_namespace_path(.packageName)
  library <- if (file.exists(file.path(nspath, "Meta/package.rds")))
    dirname(nspath)
  else
    renv_libpaths_default()

  # for R CMD check
  name <- .packageName
  pid <- Sys.getpid()

  code <- inject({
    client <- list(pid = pid, port = port)
    host <- loadNamespace(name, lib.loc = library)
    renv <- if (!is.null(host$renv)) host$renv else host
    renv$renv_watchdog_server_start(client)
  })

  writeLines(deparse(code), con = script)

  # debug logging
  debugging <- Sys.getenv("RENV_WATCHDOG_DEBUG", unset = "FALSE")
  stdout <- stderr <- if (truthy(debugging)) "" else FALSE

  # launch the watchdog
  local({
    renv_scope_envvars(RENV_PROCESS_TYPE = "watchdog-server")
    system2(
      command = R(),
      args = c("--vanilla", "-s", "-f", renv_shell_path(script)),
      stdout = stdout,
      stderr = stderr,
      wait = FALSE
    )
  })

  # wait for connection from watchdog server
  dlog("watchdog", "watchdog process launched; waiting for message")
  conn <- catch(renv_socket_accept(socket, open = "rb", timeout = 10L))
  if (inherits(conn, "error")) {
    dlog("watchdog", paste("error connecting to watchdog:", conditionMessage(conn)))
    return(FALSE)
  }

  # store information about the running process
  defer(close(conn))
  the$watchdog_process <- unserialize(conn)

  # return TRUE to indicate process was started
  dlog("watchdog", "watchdog message received [pid == %i]", the$watchdog_process$pid)
  TRUE

}

renv_watchdog_notify <- function(method, data = list()) {

  tryCatch(
    renv_watchdog_notify_impl(method, data),
    error = warnify
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
    error = warnify
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
  the$watchdog_process$pid
}

renv_watchdog_port <- function() {
  the$watchdog_process$port
}

renv_watchdog_running <- function() {
  pid <- renv_watchdog_pid()
  !is.null(pid) && renv_process_exists(pid)
}

renv_watchdog_unload <- function() {
  renv_watchdog_shutdown()
}

renv_watchdog_terminate <- function() {
  pid <- renv_watchdog_pid()
  renv_process_kill(pid)
}

renv_watchdog_shutdown <- function() {

  # nothing to do if watchdog isn't running
  if (!renv_watchdog_running())
    return(TRUE)

  # tell watchdog to shutdown
  renv_watchdog_notify("Shutdown")

  # wait for process to exit (avoid RStudio bomb)
  clock <- timer()
  wait_until(function() {
    !renv_watchdog_running() || clock$elapsed() > 1
  })

  if (!renv_watchdog_running())
    return(TRUE)

  # if it's still running, explicitly terminate it
  renv_watchdog_terminate()

  # wait for process to exit (avoid RStudio bomb)
  clock <- timer()
  wait_until(function() {
    !renv_watchdog_running() || clock$elapsed() > 1
  })

}
