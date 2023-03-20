
`_renv_exit_handlers` <- new.env(parent = emptyenv())
`_renv_exit_handlers_id` <- 1L

renv_exit_handlers_key <- function(envir) {

  # check for existing id
  id <- attr(envir, "__renv_exit_handlers_id__", exact = TRUE)
  if (!is.null(id))
    return(id)

  # set id if null
  `_renv_exit_handlers_id` <<- `_renv_exit_handlers_id` + 1L
  attr(envir, "__renv_exit_handlers_id__") <- as.character(`_renv_exit_handlers_id`)

}

renv_exit_handlers_get <- function(envir) {
  key <- renv_exit_handlers_key(envir)
  `_renv_exit_handlers`[[key]]
}

renv_exit_handlers_set <- function(envir, handlers) {

  oldhandlers <- renv_exit_handlers_get(envir)
  if (is.null(oldhandlers)) {
    call <- as.call(list(renv_exit_handlers_execute, envir))
    do.call(base::on.exit, list(substitute(call), TRUE), envir = envir)
  }

  key <- renv_exit_handlers_key(envir)
  `_renv_exit_handlers`[[key]] <- handlers

}

renv_exit_handlers_remove <- function(envir) {
  key <- renv_exit_handlers_key(envir)
  `_renv_exit_handlers`[[key]] <- NULL
}

renv_exit_handlers_execute <- function(envir) {
  handlers <- renv_exit_handlers_get(envir)
  for (handler in handlers)
    tryCatch(eval(handler$expr, handler$envir), error = identity)
  renv_exit_handlers_remove(envir)
}

renv_exit_handlers_add <- function(envir, handler) {
  handlers <- c(list(handler), renv_exit_handlers_get(envir))
  renv_exit_handlers_set(envir, handlers)
  handler
}

renv_exit_handlers_task <- function() {
  `_renv_exit_handlers_id` <<- 1L
}
