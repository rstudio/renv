
new <- function(expr) {

  private <- new.env(parent = renv_envir_self())
  public  <- new.env(parent = private)

  for (expr in as.list(substitute(expr))[-1L]) {

    assigning <-
      is.call(expr) &&
      is.symbol(expr[[1L]]) &&
      as.character(expr[[1L]]) %in% c("=", "<-")

    if (!assigning)
      return(eval(expr, envir = public))

    hidden <-
      is.symbol(expr[[2L]]) &&
      substring(as.character(expr[[2L]]), 1L, 1L) == "."

    eval(expr, envir = if (hidden) private else public)

  }

  public

}
