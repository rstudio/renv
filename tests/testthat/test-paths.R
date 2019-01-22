
context("Paths")

test_that("all renv paths live within tempdir() during tests", {
  info <- c(root = renv_paths_root(), tempdir = tempdir())
  expect_true(path_within(renv_paths_root(), tempdir()), info = info)
})
