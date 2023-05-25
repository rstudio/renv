
teardown_env <- function() {
  if (is.null(renv_namespace_load("testthat")$testthat_env$teardown_env)) {
    globalenv()
  } else {
    testthat::teardown_env()
  }
}
