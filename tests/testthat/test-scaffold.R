
context("Scaffold")

test_that("renv.lock is created when scaffold is called", {
  renv_tests_scope()
  scaffold()
  expect_true(file.exists("renv.lock"))
})
