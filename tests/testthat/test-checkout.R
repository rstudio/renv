
test_that("we can check out packages from our local repository", {

  # enter test scope
  project <- renv_tests_scope("breakfast")

  # check out a package + its dependencies; this invocation is
  # similar in spirit to a plain `install()` call
  checkout(packages = "breakfast")

  # check that they were installed
  expect_true(renv_package_installed("breakfast"))
  expect_true(renv_package_installed("bread"))

})

test_that("we can check out packages from the package manager instance", {
  skip_on_cran()
  skip_if(Sys.info()[["machine"]] == "aarch64")

  renv_tests_scope()
  init()

  # ensure we reset repos on exit
  renv_scope_options(repos = getOption("repos"))

  # install rlang from an old snapshot
  checkout(date = "2023-01-02", packages = "rlang")

  # check that we installed the requested version
  expect_true(renv_package_installed("rlang"))
  expect_true(renv_package_version("rlang") == "1.0.6")
})
