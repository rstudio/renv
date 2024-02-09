
# given a call of the form e.g. 'pkg::foo()' or 'foo()',
# check that method 'foo()' is truly being called and
# strip off the 'pkg::' part for easier parsing
renv_call_expect <- function(node, package, methods) {

  if (!is.call(node))
    return(NULL)

  # check for call of the form 'pkg::foo(a, b, c)'
  colon <- renv_call_matches(
    call  = node[[1L]],
    name  = c("::", ":::"),
    nargs = 2L
  )

  if (colon) {

    # validate the package name
    lhs <- node[[1L]][[2L]]
    if (as.character(lhs) != package)
      return(NULL)

    # extract the inner call
    rhs <- node[[1L]][[3L]]
    node[[1L]] <- rhs
  }

  # check for method match
  match <-
    is.name(node[[1L]]) &&
    as.character(node[[1L]]) %in% methods

  if (!match)
    return(NULL)

  node

}

renv_call_normalize <- function(node, stack) {

  # check for magrittr pipe -- if this part of the expression is
  # being piped into, then we need to munge the call
  ispipe <- renv_call_matches(node, name = c("%>%", "%T>%", "%<>%"))

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


renv_call_matches <- function(call, name = NULL, nargs = NULL) {

  if (!is.call(call))
    return(FALSE)

  if (!is.null(name)) {

    if (!is.name(call[[1]]))
      return(FALSE)

    if (!as.character(call[[1]]) %in% name)
      return(FALSE)

  }

  if (!is.null(nargs) && length(call) != nargs + 1L)
    return(FALSE)

  TRUE
}
