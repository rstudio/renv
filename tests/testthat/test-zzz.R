
test_that("we can clean up the userdir after", {

  # remove any leftover renv-test- directories in the userdir
  userdir <- renv_bootstrap_user_dir()
  libdir <- file.path(userdir, "library")
  testdirs <- list.files(
    path = libdir,
    pattern = "^renv-test-",
    full.names = TRUE
  )

  unlink(testdirs, recursive = TRUE)

  # if the 'library' directory itself is empty now, remove it
  libfiles <- list.files(libdir, all.files = TRUE, no.. = TRUE)
  if (empty(libfiles))
    unlink(libdir, recursive = TRUE)

})
