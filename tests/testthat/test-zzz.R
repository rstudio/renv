
context("Cleanup")

test_that("we can clean up the cache directory", {

  userdir <- renv_bootstrap_user_dir()
  libdir <- file.path(userdir, "library")
  testlibs <- list.files(libdir, pattern = "^renv-test-", full.names = TRUE)
  unlink(testlibs, recursive = TRUE)

  testlibs <- list.files(libdir, pattern = "^renv-test-", full.names = TRUE)
  expect_length(testlibs, 0L)

})
