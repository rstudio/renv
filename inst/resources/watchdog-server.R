
# list of acquired locks
..locks.. <- list()

# utility
catf <- function(fmt, ...) {
  msg <- sprintf(fmt, ...)
  cat(msg, sep = "\n")
}

# read parent process ID
ppid <- as.integer(Sys.getenv("RENV_WATCHDOG_PPID", unset = NA))
if (is.na(ppid))
  stop("internal error: RENV_WATCHDOG_PPID is unset")

# open server socket on random port
catf("[watchdog] Searching for open port for server socket.")

repeat {

  port <- sample(49152:65536, size = 1L)
  socket <- tryCatch(serverSocket(port), error = identity)
  if (inherits(socket, "error"))
    next

  break

}

catf("[watchdog] Listening on port %i", port)

# communicate information about this process to parent
metadata <- list(pid = Sys.getpid(), port = port)
pport <- as.integer(Sys.getenv("RENV_WATCHDOG_PORT"))
conn <- socketConnection(port = pport, blocking = TRUE)
serialize(metadata, connection = conn)
close(conn)

# start listening for connections
repeat {

  # check for broken socket
  if (!isOpen(socket)) {
    catf("[watchdog] Socket was unexpectedly closed; exiting now.")
    break
  }

  # check for parent exit
  nice <- tools::psnice(ppid)
  if (is.na(nice)) {
    catf("[watchdog] Parent exited; shutting down.")
    lapply(names(..locks..), unlink, recursive = TRUE, force = TRUE)
    break
  }

  # wait for connection
  conn <- tryCatch(
    socketAccept(socket, blocking = TRUE, timeout = 1L),
    error = identity
  )

  # unfortunately, we don't get nicely-classed errors for timeouts,
  # so we just suppress logging of errors altogether
  if (inherits(conn, "error"))
    next

  # read the request
  catf("[watchdog] Received connection; reading data.")
  request <- unserialize(conn)

  catf("[watchdog] Received request.")
  str(request)

  if (identical(request$method, "ListLocks")) {
    catf("[watchdog] %i lock(s) are currently held.", length(..locks..))
    str(..locks..)
    next
  }

  if (identical(request$method, "LockAcquired")) {
    catf("[watchdog] Acquired lock on path '%s'.", request$data$path)
    ..locks..[[request$data$path]] <- TRUE
    next
  }

  if (identical(request$method, "LockReleased")) {
    catf("[watchdog] Released lock on path '%s'.", request$data$path)
    ..locks..[[request$data$path]] <- FALSE
    next
  }

}
