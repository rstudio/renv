
context("Consent")

test_that("init fails if no consent has been provided", {
  renv_tests_scope()
  renv_scope_options(renv.consent = FALSE)
  expect_error(renv::init())
})
