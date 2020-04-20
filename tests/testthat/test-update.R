
context("Update")

test_that("update() finds packages requiring updates from CRAN", {
  skip_on_cran()

  renv_tests_scope()
  renv::init()

  renv::install("breakfast@0.1.0")
  expect_true(renv_package_version("breakfast") == "0.1.0")

  local({
    renv_scope_sink()
    renv::update()
  })

  expect_true(renv_package_version("breakfast") == "1.0.0")

})

test_that("update() can upgrade GitHub packages", {

  skip_on_cran()
  skip_if(getRversion() < "3.5.3")
  skip_if(is.na(Sys.getenv("GITHUB_PAT", unset = NA)))

  renv_tests_scope()
  renv::init()

  # download old commit from GitHub and track master
  renv::install("kevinushey/skeleton@5fd5d3bc616794f869e47fdf3a8b4bcaa2afcf53")

  pkgpath <- find.package("skeleton")
  descpath <- file.path(pkgpath, "DESCRIPTION")

  dcf <- renv_dcf_read(descpath)
  expect_true(dcf$Version == "0.0.0.9000")

  remotes <- dcf[grep("^Remote", names(dcf))]
  remotes$RemoteRef <- "master"
  renv_package_augment(pkgpath, remotes)

  # try updating
  update(packages = "skeleton")

  # check for new version of package
  dcf <- renv_dcf_read(descpath)
  expect_true(dcf$Version == "1.0.1")
})
