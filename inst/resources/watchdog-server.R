
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

# read port
port <- as.integer(Sys.getenv("RENV_WATCHDOG_PORT", unset = NA))
if (is.na(port))
  stop("internal error: RENV_WATCHDOG_PORT is unset")

# communicate information about this process to parent
metadata <- list(pid = Sys.getpid())
conn <- socketConnection(port = port, open = "w+b", timeout = 10)
serialize(metadata, connection = conn)
close(conn)

# start listening for connections
repeat {

  # check for parent exit
  nice <- tools::psnice(ppid)
  if (is.na(nice)) {
    catf("[watchdog] Parent exited; shutting down.")
    lapply(names(..locks..), unlink, recursive = TRUE, force = TRUE)
    break
  }

  # set file time on owned locks, so we can see they're not orphaned
  now <- Sys.time()
  lapply(names(..locks..), Sys.setFileTime, time = now)

  # wait for connection
  conn <- tryCatch(
    socketConnection(port = port, open = "rb", blocking = TRUE, timeout = 1),
    error = identity
  )

  # unfortunately, we don't get nicely-classed errors for timeouts,
  # so we just suppress logging of errors altogether
  if (inherits(conn, "error"))
    next

  # read the request
  catf("[watchdog] Received connection; reading data.")
  request <- tryCatch(unserialize(conn), error = identity)
  if (inherits(request, "error")) {
    warning(request)
    next
  }

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
