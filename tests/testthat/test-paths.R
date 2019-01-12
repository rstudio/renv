context("Paths")

test_that("all renv paths live within tempdir() during tests", {

  expect_true(path_within(renv_paths_root(), tempdir()))

})
