
context("Version")

test_that("various versions can be compared", {

  expect_equal(renv_version_compare("3.5",   "3.5.7"), -1L)
  expect_equal(renv_version_compare("3.5.0", "3.5.7"), -1L)
  expect_equal(renv_version_compare("3.5.7", "3.5.7"), +0L)
  expect_equal(renv_version_compare("3.5.8", "3.5.7"), +1L)
  expect_equal(renv_version_compare("3.10.1", "4"), -1L)

})
