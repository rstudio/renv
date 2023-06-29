
test_that("renv_metadata_version_friendly gives user friendly output", {

  expect_equal(
    renv_metadata_version_friendly(list(version = "1.0.0")),
    "1.0.0"
  )

  expect_equal(
    renv_metadata_version_friendly(list(version = structure("1.0.0", sha = "abcd1234"))),
    "1.0.0 [sha: abcd123]"
  )

})
