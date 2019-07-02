
context("Repositories")

test_that("we can query our local repository during tests", {
  expected <- list.files("packages")
  renv_tests_scope()
  ap <- renv_available_packages(type = "source")[[1]]
  expect_setequal(ap$Package, expected)
})
