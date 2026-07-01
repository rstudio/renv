
the$lock_registry <- new.env(parent = emptyenv())

renv_lock_acquire <- function(path) {

  # normalize path
  path <- renv_lock_path(path)
  dlog("lock", "%s [acquiring lock]", renv_path_pretty(path))

  # if we already have this lock, increment our counter
  count <- the$lock_registry[[path]] %||% 0L
  if (count > 0L) {
    the$lock_registry[[path]] <- count + 1L
    return(TRUE)
  }

  # make sure parent directory exists
  ensure_parent_directory(path)

  # suppress warnings in this scope
  renv_scope_options(warn = -1L)

  # loop until we acquire the lock
  repeat tryCatch(
    renv_lock_acquire_impl(path) && break,
    error = function(cnd) Sys.sleep(0.2)
  )

  # mark this path as locked by us
  the$lock_registry[[path]] <- 1L

  # notify the watchdog
  renv_watchdog_notify("LockAcquired", list(path = path))

  # TRUE to mark successful lock
  dlog("lock", "%s [lock acquired]", renv_path_pretty(path))
  TRUE

}

# https://rcrowley.org/2010/01/06/things-unix-can-do-atomically.html
renv_lock_acquire_impl <- function(path) {

  # check for orphaned locks
  if (renv_lock_orphaned(path)) {
    dlog("lock", "%s: removing orphaned lock", path)
    unlink(path, recursive = TRUE, force = TRUE)
  }

  # attempt to create the lock
  created <- dir.create(path, mode = "0755", showWarnings = FALSE)

  # if we created the lock, record its owner so that other processes can
  # tell whether the lock is still held by a live process on this machine
  if (created)
    renv_lock_owner_write(path)

  created

}

# record the host + process that owns a lock, so that renv_lock_orphaned()
# can check whether the owning process is still alive rather than relying
# solely on the lock's timestamp being kept fresh by the watchdog
renv_lock_owner_write <- function(path) {

  contents <- c(
    sprintf("Host: %s", renv_platform_nodename()),
    sprintf("Pid: %i", Sys.getpid())
  )

  owner <- file.path(path, "owner")
  catchall(writeLines(contents, con = owner))

}

renv_lock_owner_read <- function(path) {

  owner <- file.path(path, "owner")
  if (!file.exists(owner))
    return(NULL)

  props <- catch(renv_properties_read(owner))
  if (inherits(props, "error"))
    return(NULL)

  host <- props[["Host"]]
  pid <- suppressWarnings(as.integer(props[["Pid"]]))
  if (is.null(host) || is.na(pid))
    return(NULL)

  list(host = host, pid = pid)

}

renv_lock_release <- function(path) {

  # normalize path
  path <- renv_lock_path(path)

  # decrement our lock count
  count <- the$lock_registry[[path]] <- the$lock_registry[[path]] - 1L

  # remove the lock if we have no more locks
  if (count == 0L) {
    dlog("lock", "%s [lock released]", renv_path_pretty(path))
    renv_lock_release_impl(path)
  }

}

renv_lock_release_impl <- function(path) {
  unlink(path, recursive = TRUE, force = TRUE)
  remaining <- intersect(path, ls(envir = the$lock_registry, all.names = TRUE))
  rm(list = remaining, envir = the$lock_registry)
  renv_watchdog_notify("LockReleased", list(path = path))
}

renv_lock_orphaned <- function(path) {

  timeout <- getOption("renv.lock.timeout", default = 60L)
  if (timeout <= 0L)
    return(TRUE)

  info <- renv_file_info(path)
  if (is.na(info$isdir))
    return(FALSE)

  # if the lock records an owner on this host and that process is still alive,
  # the lock is not orphaned -- even if its timestamp is stale. this keeps a
  # live but slow lock holder (for example, a large package copy onto a shared
  # network cache) from having its lock stolen when the watchdog isn't
  # refreshing the lock's timestamp.
  #
  # NOTE: we only trust a *positive* liveness result. renv_process_exists() can
  # report FALSE for a live process we don't have permission to inspect (for
  # example, another user's process on a shared machine), and treating that as
  # orphaned would let us steal a live holder's lock -- the very race that
  # corrupts a shared cache. so anything other than a definite "alive" falls
  # through to the timeout below, which is no worse than the previous behavior.
  owner <- renv_lock_owner_read(path)
  if (!is.null(owner) && identical(owner$host, renv_platform_nodename())) {
    alive <- catch(renv_process_exists(owner$pid))
    if (isTRUE(alive))
      return(FALSE)
  }

  # otherwise (no owner recorded, a lock held on another host, an owner we
  # can't confirm is alive) fall back to treating the lock as orphaned once its
  # timestamp becomes stale
  diff <- difftime(Sys.time(), info$mtime, units = "secs")
  diff >= timeout

}

renv_lock_refresh <- function(lock) {
  Sys.setFileTime(lock, Sys.time())
}

renv_lock_unload <- function() {
  locks <- ls(envir = the$lock_registry, all.names = TRUE)
  unlink(locks, recursive = TRUE, force = TRUE)
}

renv_lock_path <- function(path) {

  file.path(
    renv_path_normalize(dirname(path), mustWork = TRUE),
    basename(path)
  )

}
