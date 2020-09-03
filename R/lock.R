
`_renv_locks` <- new.env(parent = emptyenv())

renv_lock_path <- function(project = NULL) {
  project <- renv_project_resolve(project)
  file.path(project, "renv/lock")
}

# NOTE: renv's locks are recursive process locks; once a given process
# has acquired a lock, it will hold onto that lock for as long as necessary
renv_lock_create <- function(path = NULL) {

  # resolve lock path
  path <- path %||% renv_lock_path()

  # ensure path normalized + exists
  path <- file.path(
    normalizePath(dirname(path), winslash = "/", mustWork = TRUE),
    basename(path)
  )

  # check for an existing id file; if it exists and we own it
  # then return an empty callback (indicating success but we previously
  # owned the lock)
  idpath <- file.path(path, "id")
  haslock <- exists(path, envir = `_renv_locks`) && file.exists(idpath)
  if (haslock) {
    id <- readLines(idpath)
    if (identical(id, `_renv_locks`[[path]]))
      return(function() {})
  }

  # try to create the lock (catch warnings / errors and treat them as failures)
  # TODO: how long should we wait here?
  i <- 1L

  while (TRUE) {

    # try to create the lock (exit on success)
    status <- catchall(dir.create(path))
    if (identical(status, TRUE))
      break

    # notify the user on the third attempt
    if (i == 3L) {
      fmt <- "* Another process is currently using renv in this project -- please wait a moment ..."
      vwritef(fmt)
    }

    # couldn't create the lock; sleep for a bit and try again
    Sys.sleep(1L)

    # update iterator
    i <- i + 1L

    # break after 30 minutes
    if (i == 300L)
      break

  }

  # check that we succeeded above
  if (!identical(status, TRUE)) {
    fmt <- "internal error: failed to acquire project lock"
    stop(fmt)
  }

  # validate that we created the lock
  if (!file.exists(path)) {
    msg <- "internal error: dir.create() returned TRUE but lock was not created"
    stop(msg)
  }

  # generate and write a UUID so we can confirm we are indeed
  # the owner of this lock when checking later
  id <- renv_id_generate()
  writeLines(id, con = file.path(path, "id"))
  assign(path, id, envir = `_renv_locks`)

  # we have created our lock: provide a callback to the caller that
  # can be used to destroy the lock
  function() { renv_lock_destroy(path) }

}

renv_lock_destroy <- function(path) {

  # remove the lockfile
  unlink(path, recursive = TRUE)

  # remove our lock cache entry
  rm(list = path, envir = `_renv_locks`)

  TRUE

}
