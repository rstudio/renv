
context("Profiles")

test_that("library paths set in a user profile are overridden after load", {
  skip_on_cran()
  skip_on_appveyor()

  renv_tests_scope()

  renv::init()
  renv_imbue_impl(project = getwd(), force = TRUE)

  profile <- c(
    ".libPaths('.')",
    "source('renv/activate.R')"
  )
  writeLines(profile, con = ".Rprofile")

  # ensure profile is executed
  renv_scope_envvars(R_PROFILE_USER = NULL)

  # invoke R
  args <- c("--slave", "-e", shQuote("writeLines(.libPaths(), 'libpaths.txt')"))
  output <- system2(R(), args, stdout = FALSE, stderr = FALSE)

  actual <- readLines("libpaths.txt")
  expected <- renv_libpaths_all()

  expect_equal(actual[[1]], expected[[1]])

})

test_that(".First is executed; library paths are restored after", {
  skip_on_cran()
  skip_on_appveyor()

  renv_tests_scope()

  renv::init()
  renv_imbue_impl(project = getwd(), force = TRUE)

  # add a .First to the profile
  profile <- c(
    ".First <- function() .libPaths('.')",
    "source('renv/activate.R')"
  )
  writeLines(profile, con = ".Rprofile")

  # ensure profile is executed
  renv_scope_envvars(R_PROFILE_USER = NULL)

  # invoke R
  args <- c("-e", shQuote("writeLines(.libPaths(), 'libpaths.txt')"))
  output <- system2(R(), args, stdout = FALSE, stderr = FALSE)
  expect_equal(output, 0L)

  actual <- readLines("libpaths.txt")
  expected <- renv_libpaths_all()

  expect_equal(actual[[1]], expected[[1]])

})
