
# TODO: This performs rlang-style injection, but doesn't
# fix up the parse tree as rlang does -- since calls like
#
#    !!a + !!b
#
# is handled by rlang as though it were parsed as
#
#    `+`(!!a, !!b)
#
# but this is not how the regular R precedence rules parse
# these expressions. Should we just use bquote()?
expr <- function(expr, envir = parent.frame()) {
  renv_expr_impl(substitute(expr), envir)
}

renv_expr_impl <- function(expr, envir) {

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
