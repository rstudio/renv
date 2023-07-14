
expect_same_elements <- function(lhs, rhs) {

  if (!requireNamespace("testthat", quietly = TRUE))
    stop("testthat not available for testing")

  if (is.list(lhs) && is.list(rhs)) {
    lhs <- lhs[order(names(lhs))]
    rhs <- rhs[order(names(rhs))]
    return(testthat::expect_equal(!!lhs, !!rhs))
  }

  if (packageVersion("testthat") > "2.2.0")
    testthat::expect_setequal(!!lhs, !!rhs)
  else
    testthat::expect_setequal(lhs, rhs)

}
