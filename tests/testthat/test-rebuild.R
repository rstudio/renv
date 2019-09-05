
context("Rebuild")

test_that("rebuild forces a package to rebuilt, bypassing cache", {

  renv_tests_scope("bread")
  init()

  records <- renv::rebuild("bread")
  expect_length(records, 1L)

})
