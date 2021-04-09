
renv_version_equal <- function(lhs, rhs, idx = NULL) {

  lhs <- numeric_version(lhs)
  rhs <- numeric_version(rhs)

  if (is.null(idx))
    lhs == rhs
  else
    lhs[1, idx] == rhs[1, idx]

}
