
# environment hosting exit callbacks
`_renv_defer_callbacks` <- new.env(parent = emptyenv())

# counter used for mapping environments to the callbacks
`_renv_defer_id` <- 1L

defer <- function(expr, envir = parent.frame()) {

  handler <- renv_defer_add(
    list(expr = substitute(expr), envir = parent.frame()),
    envir = envir
  )

  invisible(handler)

}

deferred_run <- function(envir = parent.frame()) {
  renv_defer_execute(envir)
}

renv_defer_id <- function(envir) {

  # check for existing id
  id <- attr(envir, "__renv_defer_id__", exact = TRUE)
  if (!is.null(id))
    return(id)

  # no id; bump the global count and add a new id
  `_renv_defer_id` <<- `_renv_defer_id` + 1L
  attr(envir, "__renv_defer_id__") <- as.character(`_renv_defer_id`)

}

renv_defer_get <- function(envir) {
  id <- renv_defer_id(envir)
  `_renv_defer_callbacks`[[id]]
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
  `_renv_defer_callbacks`[[id]] <- handlers

}

renv_defer_remove <- function(envir) {

  # get environment id
  id <- renv_defer_id(envir)
  if (is.null(id))
    stopf("internal error: %s has no id", format(envir))

  # remove our stored handlers
  if (exists(id, envir = `_renv_defer_callbacks`, inherits = FALSE)) {
    rm(list = id, envir = `_renv_defer_callbacks`)
  }

  # unset the handler id on the environment
  attr(envir, "__renv_defer_id__") <- NULL

}

renv_defer_execute <- function(envir) {
  handlers <- renv_defer_get(envir)
  for (handler in handlers)
    tryCatch(eval(handler$expr, handler$envir), error = identity)
  renv_defer_remove(envir)
}

renv_defer_add <- function(envir, handler) {
  handlers <- c(list(handler), renv_defer_get(envir))
  renv_defer_set(envir, handlers)
  handler
}
