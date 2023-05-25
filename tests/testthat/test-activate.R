
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


test_that("renv can bootstrap a dev version", {

  skip_slow()

  renv_tests_scope()
  init(bare = TRUE, restart = FALSE)
  renv_infrastructure_write_activate(version = "5049cef8a")

  args <- c("-s", "-e", shQuote("library(renv, warn.conflicts = FALSE)"))
  stdout <- stderr <- if (interactive()) "" else FALSE
  status <- system2(R(), args, stdout = stdout, stderr = stderr)
  expect_equal(status, 0L)

  # And should be fine if we run it again
  status <- system2(R(), args, stdout = stdout, stderr = stderr)
  expect_equal(status, 0L)
})
