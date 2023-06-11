
# don't defer warnings
options(warn = 1)

# list of acquired locks
locks <- list()

# utility
catf <- function(fmt, ...) {
  msg <- sprintf(fmt, ...)
  cat(msg, sep = "\n")
}

# information passed from the parent
parent <- list(
  pid  = as.integer(Sys.getenv("RENV_WATCHDOG_PPID", unset = NA)),
  port = as.integer(Sys.getenv("RENV_WATCHDOG_PORT", unset = NA))
)

# create socket server
for (i in 1:100) {
  port <- sample(49152:65536, size = 1L)
  socket <- tryCatch(serverSocket(port), error = identity)
  if (!inherits(socket, "error"))
    break
}

catf("[watchdog] Listening on port %i.", port)

# communicate information about this process to parent
metadata <- list(port = port, pid = Sys.getpid())
conn <- socketConnection(port = parent$port, blocking = TRUE, open = "a+b")
serialize(metadata, connection = conn)
close(conn)

# start listening for connections
repeat {

  # check for parent exit
  nice <- tools::psnice(parent$pid)
  if (is.na(nice)) {
    catf("[watchdog] Parent exited; shutting down.")
    lapply(names(locks), unlink, recursive = TRUE, force = TRUE)
    break
  }

  # set file time on owned locks, so we can see they're not orphaned
  now <- Sys.time()
  lapply(names(locks), Sys.setFileTime, time = now)

  # wait for connection
  conn <- tryCatch(
    socketAccept(socket, open = "a+b", blocking = TRUE, timeout = 1),
    condition = identity
  )

  # unfortunately, we don't get nicely-classed errors for timeouts,
  # so we just suppress logging of errors altogether
  if (inherits(conn, "condition")) {
    catf("[watchdog] Error waiting for connection: %s", conditionMessage(conn))
    next
  }

  # read the request
  catf("[watchdog] Received connection; reading data.")
  request <- tryCatch(unserialize(conn), error = identity)
  if (inherits(request, "error")) {
    catf("[watchdog] Error reading from connection: %s", conditionMessage(request))
    close(conn)
    next
  }

  catf("[watchdog] Received request.")
  str(request)

  if (identical(request$method, "ListLocks")) {
    catf("[watchdog] %i lock(s) are currently held.", length(locks))
    str(locks)
    next
  }

  if (identical(request$method, "LockAcquired")) {
    catf("[watchdog] Acquired lock on path '%s'.", request$data$path)
    locks[[request$data$path]] <- TRUE
    next
  }

  if (identical(request$method, "LockReleased")) {
    catf("[watchdog] Released lock on path '%s'.", request$data$path)
    locks[[request$data$path]] <- FALSE
    next
  }

}
