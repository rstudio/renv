
test_that("renv can bootstrap itself if not installed", {

  skip_on_cran()
  skip_on_os("windows")

  # initialize bare project
  renv_tests_scope()
  init(bare = TRUE)

  # simulate bootstrap of renv from mock library
  renv_infrastructure_write_activate(version = "1.0.0")
  renv_scope_envvars(
    RENV_BOOTSTRAP_INSTALL_ONLY = "1",
    RENV_CONFIG_REPOS_OVERRIDE  = getOption("repos")
  )

  # perform bootstrap
  args <- c("-s", "-e", shQuote("library(renv)"))
  stdout <- stderr <- if (interactive()) "" else FALSE
  status <- system2(R(), args, stdout = stdout, stderr = stderr)
  expect_equal(status, 0L)

})

test_that("renv can activate using a github sha", {
  sha <- "5049cef8a94591b"

  renv_tests_scope()
  renv_activate_impl(version = sha, quiet = TRUE)
  expect_no_error(source("renv/activate.R"))

  snapshot()
  expect_equal(renv_activate_version_lockfile("."), sha)

})
