
test_that("restore(packages = <...>) repairs missing transitive dependencies", {
  renv_tests_scope("breakfast")
  init()

  remove("bread")
  expect_false(renv_package_installed("bread"))
  expect_true(renv_package_installed("toast"))

  restore(packages = "toast")

  expect_true(renv_package_installed("bread"))
  expect_true(renv_package_installed("toast"))
})
