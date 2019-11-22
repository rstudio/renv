
context("Load")

test_that("invalid lockfile entries are reported", {

  renv_tests_scope()
  renv_scope_options(repos = NULL)

  expect_warning(renv_load_r(getwd(), NULL))
  expect_warning(renv_load_r(getwd(), list()))
  expect_warning(renv_load_r(getwd(), list(Version = "1.0.0")))

})

test_that("renv/settings.R is sourced on load if available", {
  renv_tests_scope()
  ensure_directory("renv")
  writeLines("options(renv.test.dummy = 1)", con = "renv/settings.R")
  renv_load_settings(getwd())
  expect_equal(getOption("renv.test.dummy"), 1)
})

test_that("errors when sourcing user profile are reported", {
  renv_tests_scope()
  renv_scope_options(renv.config.user.profile = TRUE)
  profile <- renv_tempfile("renv-profile-", fileext = ".R")
  writeLines("stop(1)", con = profile)
  renv_scope_envvars(R_PROFILE_USER = profile)
  expect_warning(renv_load_profile(getwd()))
})
