
library(testthat)
library(renv, warn.conflicts = FALSE)

if (!renv:::renv_tests_supported()) {
  message("* renv does not support running tests on this platform.")
  if (!interactive()) quit(status = 0L)
}

test_check("renv")
