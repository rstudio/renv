
context("Update")

test_that("update() finds packages requiring updates from CRAN", {

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
