
defer <- function(expr, envir = parent.frame()) {

  handler <- renv_exit_handlers_add(
    list(expr = substitute(expr), envir = parent.frame()),
    envir = envir
  )

  invisible(handler)

}

deferred_run <- function(envir = parent.frame()) {
  renv_exit_handlers_execute(envir)
}
