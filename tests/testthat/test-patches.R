
context("Patches")

test_that("a bad TAR is repaired", {

  # make sure to restore old TAR (e.g. we force 'internal' on AppVeyor)
  TAR <- Sys.getenv("TAR")
  on.exit(Sys.setenv(TAR = TAR), add = TRUE)

  badtar <- "/no/such/tar"
  Sys.setenv(TAR = badtar)
  expect_warning(renv_patch_tar())
  expect_false(Sys.getenv("TAR") == badtar)

})
