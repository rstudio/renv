
context("Profile")

test_that("a profile changes the default library / lockfile path", {

  renv_tests_scope()
  renv_scope_envvars(RENV_PROFILE = "testing")

  project <- getwd()
  init()

  # NOTE: renv/profile should not be written here as we've only forced
  # activation via an environment variable and not explicitly via API
  profile <- file.path(project, "renv/local/profile")
  expect_false(file.exists(profile))

  # however, other paths should resolve relative to the active profile
  prefix <- "renv/profiles/testing"

  expect_equal(
    paths$lockfile(project = project),
    file.path(project, prefix, "renv.lock")
  )

  expect_equal(
    paths$library(project = project),
    file.path(project, prefix, "renv/library", renv_platform_prefix())
  )

  expect_equal(
    paths$settings(project = project),
    file.path(project, prefix, "renv/settings.dcf")
  )

})

test_that("profile-specific dependencies can be written", {

  renv_tests_scope()

  # initialize project with 'testing' profile
  renv_scope_envvars(RENV_PROFILE = "testing")
  init()

  # have this profile depend on 'toast'
  path <- file.path(getwd(), renv_profile_prefix(), "deps.R")
  writeLines("library(toast)", con = path)

  # validate the dependency is included
  deps <- dependencies(quiet = TRUE)
  expect_true("toast" %in% deps$Package)

  # switch to other profile
  Sys.setenv(RENV_PROFILE = "other")

  # 'toast' is no longe required
  deps <- dependencies(quiet = TRUE)
  expect_false("toast" %in% deps$Package)

})

test_that("profile-specific dependencies can be declared in DESCRIPTION", {
  renv_tests_scope()

  renv_scope_envvars(RENV_PROFILE = "testing")
  init()

  writeLines(
    "Config/renv/profiles/testing/dependencies: toast",
    con = "DESCRIPTION"
  )

  deps <- dependencies()
  expect_true("toast" %in% deps$Package)

})
