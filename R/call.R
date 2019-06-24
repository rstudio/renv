
# given a call of the form e.g. 'pkg::foo()' or 'foo()',
# check that method 'foo()' is truly being called and
# strip off the 'pkg::' part for easier parsing
renv_call_expect <- function(node, package, methods) {

  if (!is.call(node))
    return(NULL)

  # check for call of the form 'pkg::foo(a, b, c)'
  colon <-
    is.call(node[[1L]]) &&
    is.name(node[[1L]][[1L]]) &&
    as.character(node[[1L]][[1L]]) %in% c("::", ":::")

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
