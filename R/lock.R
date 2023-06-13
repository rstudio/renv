
`_renv_lock_registry` <- new.env(parent = emptyenv())

renv_lock_acquire <- function(path) {

  # normalize path
  dlog("lock", "%s: acquiring lock", renv_path_pretty(path))
  path <- renv_lock_path(path)

  # if we already have this lock, increment our counter
  count <- `_renv_lock_registry`[[path]] %||% 0L
  if (count > 0L) {
    dlog("lock", "%s: incrementing lock count to %i", renv_path_pretty(path), count + 1L)
    `_renv_lock_registry`[[path]] <- count + 1L
    return(TRUE)
  }

  # loop until we acquire the lock
  repeat {

    acquired <- tryCatch(
      renv_lock_acquire_impl(path),
      condition = function(e) FALSE
    )

    if (!acquired) {
      Sys.sleep(1)
      next
    }

    break

  }

  # mark this path as locked by us
  `_renv_lock_registry`[[path]] <- 1L

  # notify the watchdog
  renv_watchdog_notify("LockAcquired", list(path = path))

  # TRUE to mark successful lock
  dlog("lock", "%s: acquired lock", renv_path_pretty(path))
  TRUE

}

renv_lock_acquire_impl <- function(path) {

  # check for orphaned locks
  if (renv_lock_orphaned(path)) {
    dlog("lock", "%s: removing orphaned lock", path)
    unlink(path, recursive = TRUE, force = TRUE)
  }

  # make sure parent directory exists
  ensure_parent_directory(path)

  # https://rcrowley.org/2010/01/06/things-unix-can-do-atomically.html
  dir.create(path, mode = "0755")

}

renv_lock_release <- function(path) {

  # normalize path
  dlog("lock", "%s: releasing lock", renv_path_pretty(path))
  path <- renv_lock_path(path)

  # decrement our lock count
  count <- `_renv_lock_registry`[[path]] <- `_renv_lock_registry`[[path]] - 1L
  dlog("lock", "%s: decrementing lock count to %i", renv_path_pretty(path), count)

  # remove the lock if we have no more locks
  if (count == 0L) {
    dlog("lock", "%s: removing lock", renv_path_pretty(path))
    renv_lock_release_impl(path)
  }

}

renv_lock_release_impl <- function(path) {
  unlink(path, recursive = TRUE, force = TRUE)
  renv_watchdog_notify("LockReleased", list(path = path))
}

renv_lock_orphaned <- function(path) {

  timeout <- getOption("renv.lock.timeout", default = 60L)
  if (timeout <= 0L)
    return(TRUE)

  info <- renv_file_info(path)
  if (is.na(info$isdir))
    return(FALSE)

  diff <- difftime(Sys.time(), info$mtime, units = "secs")
  diff >= timeout

}

renv_lock_refresh <- function(path) {
  dlog("lock", "%s: refreshing lock", renv_path_pretty(path))
  Sys.chmod(path)
}

renv_lock_init <- function() {

  # make sure we clean up locks on exit
  reg.finalizer(`_renv_lock_registry`, function(envir) {
    locks <- ls(envir = envir, all.names = TRUE)
    unlink(locks, recursive = TRUE, force = TRUE)
  }, onexit = TRUE)

}

renv_lock_path <- function(path) {

  file.path(
    renv_path_normalize(dirname(path), mustWork = TRUE),
    basename(path)
  )

}
