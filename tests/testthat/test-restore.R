
context("Restore")

test_that("library permissions are validated before restore", {
  skip_on_os("windows")
  inaccessible <- renv_tempfile()
  dir.create(inaccessible, mode = "0100")
  renv_scope_options(renv.verbose = FALSE)
  expect_false(renv_restore_preflight_permissions(inaccessible))
})
