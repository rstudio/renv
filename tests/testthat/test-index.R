
context("Index")

test_that("the available packages index is updated and cleaned", {

  renv_tests_scope()

  # request available packages
  db <- renv_available_packages(type = "source")

  # check that an index entry was created
  idx <- index("available-packages")
  expect_length(idx, 1L)

  # check that we have two files in the index path
  root <- renv_paths_index("available-packages")
  files <- list.files(root)
  expect_length(files, 2L)

})
