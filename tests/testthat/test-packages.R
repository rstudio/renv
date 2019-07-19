
context("Packages")

test_that("remote field updates are written to both DESCRIPTION, packages.rds", {

  record <- list(
    Package    = "skeleton",
    Version    = "1.0.1",
    Source     = "local",
    RemoteUrl  = normalizePath("local/skeleton/skeleton_1.0.1.tar.gz")
  )

  renv_tests_scope()
  renv::install(packages = list(record))

  pkgpath <- renv_package_find("skeleton")

  descpath <- file.path(pkgpath, "DESCRIPTION")
  desc <- renv_description_read(descpath)

  metapath <- file.path(pkgpath, "Meta/package.rds")
  meta <- readRDS(metapath)

  expect_true(desc$RemoteType == "local")
  expect_identical(as.list(desc), as.list(meta$DESCRIPTION))

})
