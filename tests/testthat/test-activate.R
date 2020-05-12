
context("Activate")

test_that("renv can bootstrap itself if not installed", {

  skip_on_cran()
  skip_on_appveyor()

  # initialize bare project
  renv_tests_scope()
  renv::init(bare = TRUE)

  # simulate bootstrap of renv from mock library
  renv_infrastructure_write_activate(version = "1.0.0")
  renv_scope_envvars(
    RENV_BOOTSTRAP_INSTALL_ONLY = "1",
    RENV_CONFIG_REPOS_OVERRIDE  = getOption("repos")
  )

  # perform bootstrap
  args <- c("--slave", "-e", shQuote("library(renv)"))
  stdout <- stderr <- if (interactive()) "" else FALSE
  status <- system2(R(), args, stdout = stdout, stderr = stderr)
  expect_equal(status, 0L)

})
