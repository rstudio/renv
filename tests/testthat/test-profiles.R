
context("Profiles")

test_that("library paths set in a user profile are overridden after load", {
  skip_on_cran()
  skip_on_appveyor()

  renv_tests_scope()

  renv::init()
  renv_bootstrap_impl(project = getwd(), force = TRUE)

  # set up a dummy profile
  writeLines(".libPaths('.')", con = "profile.R")

  # ensure .Rprofile is loaded in this context
  renv_scope_envvars(R_PROFILE_USER = ".Rprofile")

  # invoke R
  args <- c("--slave", "-e", shQuote("writeLines(.libPaths(), 'libpaths.txt')"))
  output <- system2(R(), args, stdout = FALSE, stderr = FALSE)

  actual <- readLines("libpaths.txt")
  expected <- renv_libpaths_all()

  expect_equal(actual[[1]], expected[[1]])

})
