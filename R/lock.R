
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
  #
  # note that contention is reported by the return value rather than by a
  # signalled condition, so the retry has to be driven by that return value.
  # wrapping this in tryCatch() would also swallow the error signalled by
  # setTimeLimit(), leaving callers with no way to bound the wait.
  # https://github.com/rstudio/renv/issues/2358
  failures <- 0L
  blocked <- 0L
  probefailures <- 0L
  probeafter <- 3L
  repeat {

    status <- renv_lock_acquire_impl(path)
    if (status$acquired)
      break

    # ordinary contention and a lock path we simply cannot write to are
    # reported identically -- dir.create() just returns FALSE -- so occasionally
    # probe the parent directory to determine whether retrying could succeed.
    failures <- failures + 1L
    blocked <- if (status$blocked) blocked + 1L else 0L
    if (failures >= probeafter) {
      if (renv_lock_writable(path)) {
        if (blocked >= 3L)
          renv_lock_acquire_abort(path, status$reason)
        probefailures <- 0L
        probeafter <- if (renv_file_exists(path)) 25L else 3L
      } else {
        probefailures <- probefailures + 1L
        if (probefailures >= 3L)
          renv_lock_acquire_abort(path, status$reason)
        probeafter <- probeafter * 2L
      }
      failures <- 0L
    }

    Sys.sleep(0.2)

  }

  # mark this path as locked by us
  the$lock_registry[[path]] <- 1L

  # notify the watchdog
  renv_watchdog_notify("LockAcquired", list(path = path))

  # TRUE to mark successful lock
  dlog("lock", "%s [lock acquired]", renv_path_pretty(path))
  TRUE

}

# check whether we could create a lock at 'path' at all. used to tell ordinary
# contention -- someone else holds the lock, so waiting is the right thing --
# apart from a lock path we have no hope of writing to, where waiting is a hang.
renv_lock_writable <- function(path) {

  # the probe name is unique to this process, so (unlike the lock itself) a
  # failure to create it can never be blamed on another process
  probe <- sprintf("%s.probe-%i", path, Sys.getpid())

  unlink(probe, recursive = TRUE, force = TRUE)
  created <- dir.create(probe, mode = "0755", showWarnings = FALSE)
  unlink(probe, recursive = TRUE, force = TRUE)

  created

}

renv_lock_acquire_abort <- function(path, reason) {

  message <- sprintf("renv failed to acquire the lock at %s", renv_path_pretty(path))

  body <- c(
    if (length(reason)) paste("-", reason),
    "- renv requires write access to this path to synchronize concurrent sessions."
  )

  abort(message, body = body, class = "renv_error_lock_unwritable")

}

# https://rcrowley.org/2010/01/06/things-unix-can-do-atomically.html
renv_lock_acquire_impl <- function(path) {

  # check for orphaned locks
  orphaned <- renv_lock_orphaned(path)
  if (orphaned) {
    dlog("lock", "%s: removing orphaned lock", path)
    unlink(path, recursive = TRUE, force = TRUE)
  }

  # attempt to create the lock, retaining the warning describing why we
  # couldn't -- it's normally the only thing that explains a failure which
  # isn't just ordinary contention
  reason <- NULL
  created <- withCallingHandlers(
    dir.create(path, mode = "0755"),
    warning = function(cnd) {
      reason <<- conditionMessage(cnd)
      invokeRestart("muffleWarning")
    }
  )

  # if we created the lock, record its owner so that other processes can
  # tell whether the lock is still held by a live process on this machine
  if (created)
    renv_lock_owner_write(path)

  # a stale lock we could not remove, or a non-directory entry at the lock
  # path, cannot be resolved by retrying even when the parent is writable
  info <- renv_file_info(path)
  blocked <- !created &&
    renv_file_exists(path) &&
    (orphaned || !identical(info$isdir, TRUE))

  list(acquired = created, reason = reason, blocked = blocked)

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
