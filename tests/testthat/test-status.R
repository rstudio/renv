
context("Status")

test_that("status reports packages to be installed / changed", {

  renv_tests_scope(c("toast", "breakfast"))
  renv_scope_options(renv.config.auto.snapshot = FALSE)
  renv_scope_sink()

  init(bare = TRUE)
  snapshot()
  expect_signal(status(), class = "renv.status.used_but_not_installed")

  install("breakfast")
  expect_signal(status(), class = "renv.status.installed_but_not_recorded")
  snapshot()

  record("egg")
  expect_signal(status(), class = "renv.status.recorded_but_not_used")
  snapshot()

})

test_that("status reports packages which are used but not installed", {

  renv_tests_scope()
  renv_scope_sink()
  init()

  writeLines("library(bread)", con = "script.R")
  expect_signal(status(), class = "renv.status.used_but_not_installed")

})
