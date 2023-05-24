
test_that("renv.lock is created when scaffold is called", {
  renv_tests_scope()
  scaffold()
  expect_true(file.exists("renv.lock"))
})

test_that("scaffold() accepts project settings", {
  renv_tests_scope()
  scaffold(settings = list(ignored.packages = "A"))
  ignored <- settings$ignored.packages()
  expect_identical(ignored, "A")
})
