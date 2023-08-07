
renv_watchdog_server_start <- function(client) {

  # create socket server
  server <- renv_socket_server()
  dlog("watchdog", "Listening on port %i.", server$port)

  # communicate information back to client
  dlog("watchdog", "Waiting for client...")
  metadata <- list(port = server$port, pid = server$pid)
  conn <- renv_socket_connect(port = client$port, open = "watchdog", "b")
  serialize(metadata, connection = conn)
  close(conn)
  dlog("watchdog", "Synchronized with client.")

  # initialize locks
  lockenv <- new.env(parent = emptyenv())

  # start listening for connections
  repeat tryCatch(
    renv_watchdog_server_run(server, client, lockenv),
    error = function(e) {
      dlog("watchdog", "Error: %s", conditionMessage(e))
    }
  )

}

renv_watchdog_server_run <- function(server, client, lockenv) {

  # check for parent exit
  if (!renv_process_exists(client$pid)) {
    dlog("watchdog", "Client process has exited; shutting down.")
    renv_watchdog_server_exit(server, client, lockenv)
  }

  # set file time on owned locks, so we can see they're not orphaned
  dlog("watchdog", "Refreshing lock times.")
  locks <- ls(envir = lockenv, all.names = TRUE)
  renv_lock_refresh(locks)

  # wait for connection
  dlog("watchdog", "Waiting for connection...")
  conn <- renv_socket_accept(server$socket, open = "rb", timeout = 1)
  defer(close(conn))

  # read the request
  dlog("watchdog", "Received connection; reading data.")
  request <- unserialize(conn)

  dlog("watchdog", "Received request.")
  str(request)

  # handle the request
  switch(

    request$method %||% "<missing>",

    ListLocks = {
      dlog("watchdog", "Executing 'ListLocks' request.")
      conn <- renv_socket_connect(port = request$port, open = "watchdog", "b")
      defer(close(conn))
      locks <- ls(envir = lockenv, all.names = TRUE)
      serialize(locks, connection = conn)
    },

    LockAcquired = {
      dlog("watchdog", "Acquired lock on path '%s'.", request$data$path)
      assign(request$data$path, TRUE, envir = lockenv)
    },

    LockReleased = {
      dlog("watchdog", "Released lock on path '%s'.", request$data$path)
      rm(list = request$data$path, envir = lockenv)
    },

    Shutdown = {
      dlog("watchdog", "Received shutdown request; shutting down.")
      renv_watchdog_server_exit(server, client, lockenv)
    },

    "<missing>" = {
      dlog("watchdog", "Received request with no method field available.")
    },

    {
      dlog("watchdog", "Unknown method '%s'", request$method)
    }

  )

}

renv_watchdog_server_exit <- function(server, client, lockenv) {

  # remove any existing locks
  locks <- ls(envir = lockenv, all.names = TRUE)
  unlink(locks, recursive = TRUE, force = TRUE)

  # shut down the socket server
  close(server$socket)

  # quit
  quit(status = 0)

}
