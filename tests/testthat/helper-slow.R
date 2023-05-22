skip_slow <- function()  {
  skip_on_cran()
  skip_if_not(is_slow(), "Skipping slow test")
}

is_slow <- function() {
  identical(as.logical(Sys.getenv("RENV_TESTTHAT_SLOW", "false")), TRUE)
}

slow_test_disable <- function() {
  Sys.unsetenv("RENV_TESTTHAT_SLOW")
}

slow_test_enable <- function() {
  Sys.setenv("RENV_TESTTHAT_SLOW" = "true")
}
