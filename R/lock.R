
`_renv_lock` <- new.env(parent = emptyenv())

renv_lock_acquire <- function(path) {

  # canonicalize path
  path <- renv_path_canonicalize(path)

  # if we already have this lock, increment our counter
  count <- `_renv_lock`[[path]] %||% 0L
  if (count > 0L) {
    `_renv_lock`[[path]] <- count + 1L
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
  `_renv_lock`[[path]] <- 1L

  # TRUE to mark successful lock
  TRUE

}

renv_lock_acquire_impl <- function(path) {

  # check for orphaned locks
  if (renv_lock_orphaned(path))
    unlink(path, recursive = TRUE)

  # https://rcrowley.org/2010/01/06/things-unix-can-do-atomically.html
  dir.create(path)

}

renv_lock_release <- function(path) {

  # canonicalize path
  path <- renv_path_canonicalize(path)

  # decrement our lock count
  count <- `_renv_lock`[[path]]
  count <- count - 1L
  `_renv_lock`[[path]] <- count

  # remove the lock if we have no more locks
  if (count == 0L)
    renv_lock_release_impl(path)

}

renv_lock_release_impl <- function(path) {
  unlink(path, recursive = TRUE, force = TRUE)
}

renv_lock_orphaned <- function(path) {

  info <- renv_file_info(path)
  if (is.na(info$isdir))
    return(FALSE)

  timeout <- getOption("renv.lock.timeout", default = 3600L)
  difftime(Sys.time(), info$ctime, units = "secs") > timeout

}

renv_lock_refresh <- function(path) {
  Sys.chmod(path)
}

renv_lock_init <- function() {

  # make sure we clean up locks on exit
  reg.finalizer(`_renv_lock`, function(envir) {
    locks <- ls(envir = envir, all.names = TRUE)
    unlink(locks, recursive = TRUE, force = TRUE)
  }, onexit = TRUE)

}
