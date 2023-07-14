test_that("renv_lockfile_read gives informative error", {
  file <- renv_scope_tempfile()
  writeLines("{", file)

  expect_snapshot(renv_lockfile_read(file), error = TRUE)
})
