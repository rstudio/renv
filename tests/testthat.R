
# initialize tests
renv:::renv_tests_init()

# report some diagnostics (occasionally useful for understanding test failures)
renv:::renv_tests_diagnostics()

# run the test suite
if (requireNamespace("testthat", quietly = TRUE)) {
  result <- testthat::test_check("renv")
  renv:::renv_tests_report(result)
} else {
  message("'testthat' is not installed; test suite will not be run")
}
