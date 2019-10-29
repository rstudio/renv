
`_renv_exit_handlers` <- new.env(parent = emptyenv())

renv_exit_handlers_get <- function(envir) {
  key <- format(envir)
  `_renv_exit_handlers`[[key]]
}

renv_exit_handlers_set <- function(envir, handlers) {

  oldhandlers <- renv_exit_handlers_get(envir)
  if (is.null(oldhandlers)) {
    call <- as.call(list(renv_exit_handlers_execute, envir))
    do.call(base::on.exit, list(substitute(call), TRUE), envir = envir)
  }

  key <- format(envir)
  `_renv_exit_handlers`[[key]] <- handlers

}

renv_exit_handlers_remove <- function(envir) {
  key <- format(envir)
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
