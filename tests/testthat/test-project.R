test_that("renv_project_synchronized_check() reports if no packages installed", {

  renv_tests_scope("breakfast")
  install("bread")
  init()
  unlink("renv/library", recursive = TRUE)

  renv_scope_options(renv.verbose = TRUE)
  expect_snapshot(ok <- renv_project_synchronized_check())
  expect_false(ok)
})
