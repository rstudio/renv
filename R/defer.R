
# environments with pending exit handlers, as list(envir, handlers) entries.
# environments are matched by identity: keying a table by their formatted
# addresses would intern a new symbol, which is never freed, for each
# environment seen. each entry is updated with a single assignment, so an
# interrupt can't leave an environment paired with another's handlers
the$defer_registry <- list()

renv_defer_init <- function() {

  # make sure we run callbacks set on the global environment on exit
  # but only in non-interactive sessions, just to be safe
  if (interactive())
    return()

  reg.finalizer(.GlobalEnv, renv_defer_execute, onexit = TRUE)

}

defer <- function(expr, scope = parent.frame()) {

  handler <- renv_defer_add(
    list(expr = substitute(expr), envir = parent.frame()),
    envir = scope
  )

  invisible(handler)

}

renv_defer_index <- function(envir) {

  # search from the end, as handlers are usually added to recent scopes
  registry <- the$defer_registry
  i <- length(registry)
  while (i > 0L) {
    if (identical(registry[[i]]$envir, envir))
      return(i)
    i <- i - 1L
  }

  0L

}

renv_defer_get <- function(envir) {
  index <- renv_defer_index(envir)
  if (index > 0L)
    the$defer_registry[[index]]$handlers
}

renv_defer_remove <- function(envir) {
  index <- renv_defer_index(envir)
  if (index > 0L)
    the$defer_registry[[index]] <- NULL
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

  # handlers run in LIFO order
  index <- renv_defer_index(envir)
  if (index > 0L) {
    entry <- the$defer_registry[[index]]
    entry$handlers <- c(list(handler), entry$handlers)
    the$defer_registry[[index]] <- entry
    return(handler)
  }

  # this is the first handler for this environment, so also arrange for
  # the handlers to be run when it exits
  call <- as.call(list(renv_defer_execute, envir))
  do.call(base::on.exit, list(call, TRUE), envir = envir)

  entry <- list(envir = envir, handlers = list(handler))
  the$defer_registry[[length(the$defer_registry) + 1L]] <- entry
  handler

}
