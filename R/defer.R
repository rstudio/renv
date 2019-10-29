
defer <- function(expr, envir = parent.frame()) {
  renv_exit_handlers_add(
    list(expr = substitute(expr), envir = parent.frame()),
    envir = envir
  )
}

