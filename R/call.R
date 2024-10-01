
# given a call of the form e.g. 'pkg::foo()' or 'foo()',
# check that method 'foo()' is truly being called and
# strip off the 'pkg::' part for easier parsing.
#
# this gets called very often when parsing dependencies,
# so optimizations are welcome here
renv_call_expect <- function(node, package, methods) {

  result <- NULL
  
  # check for call of the form 'pkg::foo(a, b, c)'
  if (is.call(call <- node[[1L]]))
    if (is.symbol(symbol <- call[[1L]]))
      if (symbol == "::" || symbol == ":::")
        if (call[[2L]] == package)
          node[[1L]] <- call[[3L]]
  
  # check for any method match
  if (is.symbol(symbol <- node[[1L]]))
    if (any(symbol == methods))
      result <- node
  
  result

}

renv_call_normalize <- function(node) {

  # check for magrittr pipe -- if this part of the expression is
  # being piped into, then we need to munge the call
  ispipe <- renv_call_matches(node, names = c("%>%", "%T>%", "%<>%"))
  if (!ispipe)
    return(node)

  # get lhs and rhs of piped expression
  lhs <- node[[2L]]
  rhs <- node[[3L]]

  # handle rhs symbols
  if (is.symbol(rhs))
    rhs <- call(as.character(rhs))

  # check for usage of '.'
  # if it exists, replace each with lhs
  hasdot <- FALSE
  dot <- as.symbol(".")
  for (i in seq_along(rhs)) {
    if (identical(dot, rhs[[i]])) {
      hasdot <- TRUE
      rhs[[i]] <- lhs
    }
  }

  if (hasdot)
    return(rhs)

  # otherwise, mutate rhs call with lhs passed as first argument
  args <- as.list(rhs)
  as.call(c(args[[1L]], lhs, args[-1L]))

}


renv_call_matches <- function(call, names, nargs = NULL) {

  ok <- FALSE
  
  if (is.call(call))
    if (is.symbol(sym <- call[[1L]]))
      if (any(names == sym))
        ok <- is.null(nargs) || length(call) == nargs + 1L
  
  ok
  
}
