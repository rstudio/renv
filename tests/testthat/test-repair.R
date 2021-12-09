
context("Repair")

test_that("we can repair a broken project library", {

  skip_on_cran()
  renv_scope_options(renv.tests.verbose = FALSE)
  renv_tests_scope("breakfast")

  init()

  # find breakfast in the cache, and delete it
  record <- list(Package = "breakfast", Version = "1.0.0")
  cachepath <- renv_cache_find(record)
  unlink(cachepath, recursive = TRUE)

  # check that the package no longer exists
  expect_false(renv_package_installed("breakfast"))

  # try to repair
  repair()

  # validate that we reinstalled it
  expect_true(renv_package_installed("breakfast"))

})

test_that("repair() uses the package version recorded in the lockfile", {

  skip_on_cran()
  renv_scope_options(renv.tests.verbose = FALSE)
  renv_tests_scope("breakfast")

  init()

  # install older breakfast from archive
  install("breakfast@0.1.0")
  snapshot()

  # find breakfast in the cache, and delete it
  record <- list(Package = "breakfast", Version = "0.1.0")
  cachepath <- renv_cache_find(record)
  unlink(cachepath, recursive = TRUE)

  # check that the package no longer exists
  expect_false(renv_package_installed("breakfast"))

  # try to repair
  repair()

  # validate that we reinstalled it
  expect_true(renv_package_installed("breakfast"))
  expect_true(renv_package_version("breakfast") == "0.1.0")

})
