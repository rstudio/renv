
context("Status")

test_that("status() works when there is no library", {

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
