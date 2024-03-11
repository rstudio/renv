
expr <- function(expr, envir = parent.frame()) {
  renv_expr_impl(substitute(expr), envir)
}

renv_expr_impl <- function(expr, envir) {

  # repair parse trees
  expr <- renv_expr_repair(expr)

  # check for inject calls
  if (is.call(expr) && identical(expr[[1L]], as.symbol("!"))) {
    inner <- expr[[2L]]
    if (is.call(inner) && identical(inner[[1L]], as.symbol("!"))) {
      value <- eval(inner[[2L]], envir = envir)
      return(value)
    }
  }

  # recurse where possible
  if (is.recursive(expr)) {
    for (i in seq_along(expr)) {
      expr[[i]] <- renv_expr_impl(expr[[i]], envir)
    }
  }

  expr

}

renv_expr_extract <- function(expr) {

  if (is.call(expr) && identical(expr[[1L]], as.symbol("!"))) {
    inner <- expr[[2L]]
    if (is.call(inner) && identical(inner[[1L]], as.symbol("!"))) {
      return(inner[[2L]])
    }
  }

}

# TODO: Doesn't properly handle precedence for multiple injections,
# e.g. in '!!a + !!b + !!c'.
renv_expr_repair <- function(expr) {

  lhs <- renv_expr_extract(expr)
  if (is.null(lhs))
    return(expr)

  check <- is.call(lhs) && length(lhs) == 3L
  if (!check)
    return(expr)

  rhs <- renv_expr_extract(lhs[[3L]])
  if (is.null(rhs))
    return(expr)

  parts <- list(
    lhs[[1L]],
    call("!", call("!", lhs[[2L]])),
    call("!", call("!", rhs))
  )

  as.call(parts)

}
