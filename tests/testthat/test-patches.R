
test_that("a bad TAR is repaired", {
  renv_scope_envvars(TAR = "/no/such/tar")
  expect_warning(renv_patch_tar())
  expect_false(Sys.getenv("TAR") == "/no/such/tar")
})

test_that("loading renv leaves an unset TAR alone", {
  # https://github.com/rstudio/renv/issues/2418
  renv_scope_envvars(TAR = NULL)
  renv_patch_tar()
  expect_false(renv_envvar_exists("TAR"))
})

test_that("renv's own untar() calls prefer the internal tar on Windows", {

  renv_scope_binding(renv_envir_self(), "renv_platform_windows", function() TRUE)

  renv_scope_envvars(TAR = NULL)
  expect_equal(renv_tar_default(), "internal")

  renv_scope_envvars(TAR = "/path/to/tar")
  expect_equal(renv_tar_default(), "/path/to/tar")

})

test_that("renv's own untar() calls honor TAR on other platforms", {

  renv_scope_binding(renv_envir_self(), "renv_platform_windows", function() FALSE)

  renv_scope_envvars(TAR = NULL)
  expect_equal(renv_tar_default(), "")

  renv_scope_envvars(TAR = "/path/to/tar")
  expect_equal(renv_tar_default(), "/path/to/tar")

})
