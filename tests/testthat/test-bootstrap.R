
context("Bootstrap")

test_that("we can bootstrap the current version of renv", {

  renv_tests_scope()

  library <- renv_libpaths_default()
  bootstrap(version = "1.0.0", library = library)
  expect_true(renv_package_installed("renv", library))
  expect_true(renv_package_version("renv") == "1.0.0")

})

test_that("we can bootstrap an archived version of renv", {

  renv_tests_scope()

  library <- renv_libpaths_default()
  bootstrap(version = "0.1.0", library = library)
  expect_true(renv_package_installed("renv", library))
  expect_true(renv_package_version("renv") == "0.1.0")

})

test_that("we can install a version of renv from GitHub", {
  skip_on_cran()
  skip_on_appveyor()

  renv_tests_scope()

  library <- renv_libpaths_default()
  bootstrap(version = "0.9.3-10", library = library)
  expect_true(renv_package_installed("renv", library))
  expect_true(renv_package_version("renv") == "0.9.3-10")

})
