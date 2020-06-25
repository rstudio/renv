
context("Diagnostics")

test_that("we can create a diagnostics report", {
  skip_on_cran()
  renv_tests_scope()
  capture.output(diagnostics())
  expect_true(TRUE)
})
