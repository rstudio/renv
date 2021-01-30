
context("Profile")

test_that("a profile changes the default library / lockfile path", {

  renv_tests_scope()
  renv_scope_envvars(RENV_PROFILE = "testing")

  project <- getwd()
  init()

  # NOTE: renv/profile should not be written here as we've only forced
  # activation via an environment variable and not explicitly via API
  profile <- file.path(project, "renv/profile")
  expect_false(file.exists(profile))

  # however, other paths should resolve relative to the active profile
  prefix <- "renv/profiles/testing"

  expect_equal(
    paths$lockfile(project = project),
    file.path(project, prefix, "renv.lock")
  )

  expect_equal(
    paths$library(project = project),
    file.path(project, prefix, "renv/library", renv_prefix_platform())
  )

  expect_equal(
    paths$settings(project = project),
    file.path(project, prefix, "renv/settings.dcf")
  )

})
