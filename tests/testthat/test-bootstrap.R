
context("Bootstrap")

test_that("we can bootstrap the current version of renv", {

  skip_on_cran()
  skip_on_ci()

  renv_tests_scope()

  library <- renv_libpaths_default()
  bootstrap(version = "1.0.0", library = library)
  expect_true(renv_package_installed("renv", library))
  expect_true(renv_package_version("renv") == "1.0.0")

})

test_that("we can bootstrap an archived version of renv", {

  skip_on_cran()
  skip_on_ci()

  renv_tests_scope()

  library <- renv_libpaths_default()
  bootstrap(version = "0.1.0", library = library)
  expect_true(renv_package_installed("renv", library))
  expect_true(renv_package_version("renv") == "0.1.0")

})

test_that("we can install a version of renv from GitHub", {

  skip_on_cran()
  skip_on_ci()

  renv_tests_scope()

  library <- renv_libpaths_default()
  bootstrap(version = "0.12.3-1", library = library)
  expect_true(renv_package_installed("renv", library))
  expect_true(renv_package_version("renv") == "0.12.3-1")

})

test_that("bootstrap succeeds with empty repos", {

  skip_on_cran()
  skip_on_os("windows")

  renv_tests_scope()
  renv_scope_options(repos = character())

  library <- renv_libpaths_default()
  bootstrap(version = "1.0.0", library = library)
  expect_true(renv_package_installed("renv", library))
  expect_true(renv_package_version("renv") == "1.0.0")

})

test_that("bootstrap functions don't depend on non-bootstrap APIs", {

  # get all of the bootstrap functions defined in renv
  renv <- asNamespace("renv")
  keys <- grep("^renv_bootstrap_", ls(envir = renv), value = TRUE)
  fns <- mget(keys, envir = renv, mode = "function")
  bodies <- map(fns, body)

  # iterate over those functions and look for the called functions
  calls <- stack(mode = "character")
  recurse(bodies, function(node, stack) {
    if (is.call(node) && is.symbol(node[[1L]]))
      calls$push(as.character(node[[1L]]))
  })
  calls <- calls$data()

  # check what renv APIs are used
  apis <- grep("^renv_", calls, value = TRUE)

  # validate they're all renv_bootstrap_ APIs
  ok <- grepl("^renv_bootstrap_", apis)
  expect_true(all(ok))

})
