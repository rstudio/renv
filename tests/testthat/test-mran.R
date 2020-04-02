
context("MRAN")

test_that("older binaries are installed from MRAN on Windows / macOS", {

  skip_on_cran()
  skip_on_os("linux")

  renv_tests_scope()
  renv_scope_options(pkgType = "binary")

  version <- getRversion()[1, 1:2]
  if (version != "3.5")
    skip("only run on R 3.5")

  install("digest@0.6.17")

  path <- find.package("digest", lib.loc = .libPaths())
  desc <- renv_description_read(path)

  expect_identical(desc$Package, "digest")
  expect_identical(desc$Version, "0.6.17")

})
