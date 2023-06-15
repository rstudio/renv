
# environment hosting exit callbacks
the$defer_callbacks <- new.env(parent = emptyenv())

defer <- function(expr, scope = parent.frame()) {

  handler <- renv_defer_add(
    list(expr = substitute(expr), envir = parent.frame()),
    envir = scope
  )

  invisible(handler)

}

renv_defer_id <- function(envir) {
  format.default(envir)
}

renv_defer_get <- function(envir) {
  id <- renv_defer_id(envir)
  the$defer_callbacks[[id]]
}

renv_defer_set <- function(envir, handlers) {

  # get any previously-set handlers. if we don't see any handlers registered,
  # this must be our first time registering exit handlers on the environment,
  # and so we'll want to register an on.exit handler to call our handlers
  oldhandlers <- renv_defer_get(envir)
  if (is.null(oldhandlers)) {
    call <- as.call(list(renv_defer_execute, envir))
    do.call(base::on.exit, list(substitute(call), TRUE), envir = envir)
  }

  # register the newly-set handlers
  id <- renv_defer_id(envir)
  the$defer_callbacks[[id]] <- handlers

}

renv_defer_remove <- function(envir) {
  id <- renv_defer_id(envir)
  rm(list = id, envir = the$defer_callbacks)
}

renv_defer_execute <- function(envir = parent.frame()) {

  # check for handlers -- may be NULL if they were intentionally executed
  # early via a call to `renv_defer_execute()`
  handlers <- renv_defer_get(envir)
  if (is.null(handlers))
    return()

  # execute the existing handlers
  for (handler in handlers)
    tryCatch(eval(handler$expr, handler$envir), error = identity)

  # remove the handlers
  renv_defer_remove(envir)

}

renv_defer_add <- function(envir, handler) {
  handlers <- c(list(handler), renv_defer_get(envir))
  renv_defer_set(envir, handlers)
  handler
}
