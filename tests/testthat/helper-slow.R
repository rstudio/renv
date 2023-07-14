
skip_slow <- function()  {
  skip_on_cran()
  skip_if_not(is_slow(), "Skipping slow test")
}

is_slow <- function() {
  truthy(Sys.getenv("RENV_TESTTHAT_SLOW", unset = "false"))
}

slow_test_disable <- function() {
  Sys.unsetenv("RENV_TESTTHAT_SLOW")
}

slow_test_enable <- function() {
  Sys.setenv("RENV_TESTTHAT_SLOW" = "true")
}
