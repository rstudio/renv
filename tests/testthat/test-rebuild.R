
test_that("rebuild forces a package to rebuilt, bypassing cache", {

  renv_tests_scope("bread")
  init()

  records <- rebuild("bread")
  expect_length(records, 1L)

})

test_that("rebuild installs latest-available package if not installed", {

  renv_tests_scope()
  init()

  rebuild("bread")
  expect_true(renv_package_installed("bread"))

})
