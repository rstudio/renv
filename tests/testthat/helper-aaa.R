
teardown_env <- function() {
  if (interactive()) globalenv() else testthat::teardown_env()
}
