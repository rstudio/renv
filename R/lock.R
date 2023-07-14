
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

  # make sure warnings are errors here
  renv_scope_options(warn = 2L)

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
  dir.create(path, mode = "0755")

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
  renv_scope_options(warn = -1L)
  unlink(path, recursive = TRUE, force = TRUE)
  rm(list = path, envir = the$lock_registry)
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
