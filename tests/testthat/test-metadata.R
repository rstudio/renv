test_that("renv_metadata_is_dev works as expected", {
  expect_true(renv_metadata_is_dev(list(sha = "abc")))
  expect_true(renv_metadata_is_dev(list(version = "1.1.1.1")))
  expect_false(renv_metadata_is_dev(list(version = "1.1.1")))
})
