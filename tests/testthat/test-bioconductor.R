
context("Bioconductor")

test_that("packages can be installed, restored from Bioconductor", {

  skip_on_cran()
  skip_on_appveyor()
  skip_if(getRversion() < "3.6")

  renv_tests_scope("limma")

  cran <- "https://cloud.r-project.org"
  install.packages("BiocManager", repos = cran, quiet = TRUE)
  BiocManager::install("limma", quiet = TRUE)

  snapshot()

  lockfile <- snapshot(lockfile = NULL)
  limma <- lockfile$Packages$limma
  expect_equal(limma$Source, "Bioconductor")

  remove("limma")
  restore()
  expect_true(renv_package_installed("limma"))

})
