
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

renv_defer_key <- function(envir) {

  # check for existing id
  id <- attr(envir, "__renv_defer_id__", exact = TRUE)
  if (!is.null(id))
    return(id)

  # no id; bump the global count and add a new id
  `_renv_defer_id` <<- `_renv_defer_id` + 1L
  attr(envir, "__renv_defer_id__") <- as.character(`_renv_defer_id`)

}

renv_defer_get <- function(envir) {
  key <- renv_defer_key(envir)
  `_renv_defer_callbacks`[[key]]
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
  key <- renv_defer_key(envir)
  `_renv_defer_callbacks`[[key]] <- handlers

}

renv_defer_remove <- function(envir) {

  # remove our stored handlers
  key <- renv_defer_key(envir)
  rm(list = key, envir = `_renv_defer_callbacks`)

  # unset the handler key on the environment
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
