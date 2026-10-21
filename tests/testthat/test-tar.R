
test_that("renv_tar_r() reports the tar R's untar() would use", {

  renv_scope_envvars(TAR = "/path/to/tar --force-local")
  expect_equal(renv_tar_r(), "/path/to/tar --force-local")

  renv_scope_binding(renv_envir_self(), "renv_platform_windows", function() FALSE)
  renv_scope_envvars(TAR = NULL)
  expect_equal(renv_tar_r(), "internal")

})

test_that("renv_tar_r() finds a tar.exe on the PATH on Windows", {

  renv_scope_binding(renv_envir_self(), "renv_platform_windows", function() TRUE)
  renv_scope_envvars(TAR = NULL)

  # no tar.exe on the PATH
  renv_scope_envvars(PATH = "")
  expect_equal(renv_tar_r(), "internal")

  # a tar.exe on the PATH is reported by its resolved path
  bin <- renv_scope_tempfile("renv-bin-")
  ensure_directory(bin)
  tar <- file.path(bin, "tar.exe")
  writeLines("#!/bin/sh", tar)
  Sys.chmod(tar, "0755")

  renv_scope_envvars(PATH = bin)
  expect_equal(renv_tar_r(), renv_path_normalize(tar))

})

test_that("diagnostics describe the selected tars", {

  renv_scope_options(renv.verbose = TRUE)

  expect_equal(renv_diagnostics_tar_describe("internal"), "internal")
  expect_equal(renv_diagnostics_tar_describe("/no/such/tar"), "/no/such/tar")

  tar <- Sys.which("tar")
  skip_if(!nzchar(tar))
  expect_match(renv_diagnostics_tar_describe(tar), "^.*tar \\[.+\\]$")

  # the note is only shown when R would bypass the system tar on Windows
  renv_scope_binding(renv_envir_self(), "renv_tar_exe", function(...) "internal")
  renv_scope_binding(renv_envir_self(), "renv_tar_system", function() "C:/Windows/System32/tar.exe")

  renv_scope_binding(renv_envir_self(), "renv_tar_r", function() "C:/Windows/System32/tar.exe")
  expect_no_match(capture.output(renv_diagnostics_tar(NULL)), "NOTE")

  renv_scope_binding(renv_envir_self(), "renv_tar_r", function() "C:/Git/usr/bin/tar.exe")
  expect_match(paste(capture.output(renv_diagnostics_tar(NULL)), collapse = "\n"), "NOTE: R's untar")

})
