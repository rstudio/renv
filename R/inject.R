
inject <- function(expr, envir = parent.frame()) {
  renv_inject_impl(substitute(expr), envir)
}

renv_inject_impl <- function(expr, envir) {

  if (is.call(expr) && identical(expr[[1L]], as.symbol("."))) {
    expr <- renv_inject_impl(expr[[2L]], envir = envir)
    return(eval(expr, envir = envir))
  }

  if (is.recursive(expr))
    for (i in seq_along(expr))
      expr[[i]] <- renv_inject_impl(expr[[i]], envir)

  expr

}
