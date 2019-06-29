
context("Patches")

test_that("a bad TAR is repaired", {
  renv_scope_envvars(TAR = "/no/such/tar")
  expect_warning(renv_patch_tar())
  expect_false(Sys.getenv("TAR") == "/no/such/tar")
})
