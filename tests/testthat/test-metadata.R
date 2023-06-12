test_that("renv_metadata_is_dev works as expected", {
  expect_true(renv_metadata_is_dev(list(sha = "abc")))
  expect_true(renv_metadata_is_dev(list(version = "1.1.1.1")))
  expect_false(renv_metadata_is_dev(list(version = "1.1.1")))
})

test_that("renv_metadata_version_friendly gives user friendly output", {

  expect_equal(
    renv_metadata_version_friendly(list(version = "1.0.0")),
    "1.0.0"
  )
  expect_equal(
    renv_metadata_version_friendly(list(version = "1.0.0", sha = "abcd1234")),
    "1.0.0; rstudio/renv@abcd12"
  )

})
