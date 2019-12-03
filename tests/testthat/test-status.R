
context("Status")

test_that("status() works when there is no library", {
  skip_on_cran()
  renv_scope_options(renv.config.snapshot.preflight = FALSE)

  renv_tests_scope("breakfast")
  renv::init()

  info <- local({
    renv_scope_sink()
    renv::status()
  })

  expect_length(renv_records(info$library), 4)
  expect_length(renv_records(info$lockfile), 4)

  unlink("renv/library", recursive = TRUE)

  info <- local({
    renv_scope_sink()
    renv::status()
  })

  expect_length(renv_records(info$library), 0)
  expect_length(renv_records(info$lockfile), 4)

})

test_that("status reports packages to be installed / changed", {

  renv_tests_scope(c("toast", "breakfast"))
  renv_scope_options(renv.config.auto.snapshot = FALSE)
  renv_scope_sink()

  init(bare = TRUE)

  install("toast")
  expect_signal(status(), class = "renv.status.installed_but_not_recorded")
  snapshot()

  install("breakfast")
  remove("toast")
  expect_signal(status(), class = "renv.status.installed_but_not_recorded")
  expect_signal(status(), class = "renv.status.recorded_but_not_installed")
  snapshot(force = TRUE)

  install("breakfast@0.1.0")
  expect_signal(status(), class = "renv.status.installed_but_not_recorded")
  snapshot()

})
