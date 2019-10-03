
context("Packages")

test_that("remote field updates are written to both DESCRIPTION, packages.rds", {

  url <- renv_path_normalize("local/skeleton/skeleton_1.0.1.tar.gz")
  record <- list(
    Package    = "skeleton",
    Version    = "1.0.1",
    Source     = "local",
    RemoteUrl  = url
  )

  renv_tests_scope()
  renv::install(packages = list(record))

  pkgpath <- renv_package_find("skeleton")

  descpath <- file.path(pkgpath, "DESCRIPTION")
  desc <- renv_description_read(descpath)

  metapath <- file.path(pkgpath, "Meta/package.rds")
  meta <- as.list(readRDS(metapath)$DESCRIPTION)

  expect_true(desc$RemoteType == "local")
  expect_true(meta$RemoteType == "local")

  expect_true(desc$RemoteUrl == url)
  expect_true(meta$RemoteUrl == url)

})
