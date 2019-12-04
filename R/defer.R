
defer <- function(expr, envir = parent.frame()) {

  handler <- renv_exit_handlers_add(
    list(expr = substitute(expr), envir = parent.frame()),
    envir = envir
  )

  invisible(handler)

}

