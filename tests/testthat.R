library(testthat)
library(renv, warn.conflicts = FALSE)

if (renv:::renv_tests_supported()) {
  # renv:::renv_tests_diagnostics()
  test_check("renv")
} else {
  message("renv does not support running tests on this platform.")
}
