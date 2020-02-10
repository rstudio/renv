
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

expect_signal <- function(expr, class) {

  conditions <- stack()
  withCallingHandlers(expr, condition = function(c) conditions$push(c))

  ok <- FALSE
  for (c in conditions$data()) {
    if (inherits(c, class)) {
      ok <- TRUE
      break
    }
  }

  message <- sprintf("did not signal condition of class '%s'", class)
  testthat::expect(ok, message)

}

skip_if_local <- function() {
  ci <- Sys.getenv("CI", unset = NA)
  testthat::skip_if(is.na(ci), "Running tests locally")
}
