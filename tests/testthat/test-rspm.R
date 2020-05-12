
context("RSPM")

test_that("repository URLs are properly transformed for different platforms", {
  skip_on_cran()
  skip_on_os("windows")

  renv_scope_envvars(RENV_RSPM_OS = "__linux__", RENV_RSPM_PLATFORM = "bionic")
  repos <- c(RSPM = "https://cluster.rstudiopm.com/cran/latest")
  expected <- c(RSPM = "https://cluster.rstudiopm.com/cran/__linux__/bionic/latest")
  actual <- renv_rspm_transform(repos)
  expect_identical(expected, actual)

})

test_that("a binary-specific URL is transformed before writing a lockfile", {
  skip_on_cran()

  renv_tests_scope()
  renv_scope_options(repos = c(RSPM = "https://cluster.rstudiopm.com/cran/__os__/platform/latest"))
  lockfile <- snapshot(lockfile = NULL)
  actual <- convert(lockfile$R$Repositories, "character")
  expected <- c(RSPM = "https://cluster.rstudiopm.com/cran/latest")
  expect_identical(actual, expected)

})

test_that("RSPM bits are preserved when writing lockfile", {
  skip_on_cran()

  renv_tests_scope()
  renv_scope_options(repos = c(RSPM = "https://cluster.rstudiopm.com/curated/__linux__/bionic/12"))
  lockfile <- snapshot(lockfile = NULL)
  actual <- convert(lockfile$R$Repositories, "character")
  expected <- c(RSPM = "https://cluster.rstudiopm.com/curated/12")
  expect_identical(actual, expected)

})

test_that("RSPM is confirmed not supported on trusty", {
  skip_on_cran()
  renv_scope_envvars(RENV_RSPM_OS = "__linux__", RENV_RSPM_PLATFORM = "trusty")
  before <- "https://cluster.rstudiopm.com/cran/latest"
  after  <- renv_rspm_transform(before)
  expect_identical(unname(before), unname(after))
})
