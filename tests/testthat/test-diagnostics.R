
context("Diagnostics")

test_that("we can create a diagnostics report", {
  renv_tests_scope()
  capture.output(diagnostics())
  expect_true(TRUE)
})
