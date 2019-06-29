
context("Upgrade")

test_that("the version of renv in a project can be changed (upgraded)", {
  skip_on_cran()

  renv_tests_scope("breakfast")
  renv::init()
  renv::load()

  quietly(renv::upgrade(version = "0.5.0"))

  project <- getwd()
  expect_equal(renv_activate_version(project), "0.5.0")

})
