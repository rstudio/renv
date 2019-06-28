
context("Patches")

test_that("a bad TAR is repaired", {

  badtar <- "/no/such/tar"
  Sys.setenv(TAR = badtar)

  expect_warning(renv_patch_tar())

  expect_false(Sys.getenv("TAR") == badtar)

})
