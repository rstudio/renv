
context("Hydrate")

test_that("hydrate does not change library paths", {

  renv_tests_scope()

  lib <- renv_tempfile()
  ensure_directory(lib)
  .libPaths(lib)

  before <- .libPaths()
  renv::hydrate()
  after <- .libPaths()

  expect_identical(before, after)

})
