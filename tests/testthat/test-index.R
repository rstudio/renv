
context("Index")

test_that("the available packages index is updated and cleaned", {

  scope <- "available-packages"

  path <- renv_scope_tempfile("renv-index-")
  ensure_directory(path)
  renv_scope_envvars(RENV_PATHS_INDEX = path)

  renv_tests_scope()

  # request available packages
  db <- available_packages(type = "source")

  # check that an index entry was created
  idx <- index(scope)
  expect_length(idx, 1L)

  # check that we have two files in the index path
  root <- renv_paths_index(scope)
  files <- list.files(root)
  expect_length(files, 2L)

})
