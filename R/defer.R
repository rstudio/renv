
# environments with pending exit handlers, and those handlers. environments
# are matched by identity: keying a table by their formatted addresses would
# intern a new symbol, which is never freed, for each environment seen
the$defer_envirs <- list()
the$defer_handlers <- list()

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
  envirs <- the$defer_envirs
  i <- length(envirs)
  while (i > 0L) {
    if (identical(envirs[[i]], envir))
      return(i)
    i <- i - 1L
  }

  0L

}

renv_defer_get <- function(envir) {
  index <- renv_defer_index(envir)
  if (index > 0L)
    the$defer_handlers[[index]]
}

renv_defer_remove <- function(envir) {
  index <- renv_defer_index(envir)
  if (index > 0L) {
    the$defer_envirs[[index]] <- NULL
    the$defer_handlers[[index]] <- NULL
  }
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
    the$defer_handlers[[index]] <- c(list(handler), the$defer_handlers[[index]])
    return(handler)
  }

  # this is the first handler for this environment, so also arrange for
  # the handlers to be run when it exits
  call <- as.call(list(renv_defer_execute, envir))
  do.call(base::on.exit, list(call, TRUE), envir = envir)

  index <- length(the$defer_envirs) + 1L
  the$defer_envirs[[index]] <- envir
  the$defer_handlers[[index]] <- list(handler)
  handler

}
