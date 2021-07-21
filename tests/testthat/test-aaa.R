
context("Setup")

renv_tests_init()

test_that("RENV_PATHS_ROOT resolves to the temporary directory", {
  root <- renv_paths_root()
  expect_equal(root, file.path(tempdir(), "renv"))
})
