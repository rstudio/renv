context("Init")

test_that("we can initialize a project using 'breakfast'", {

  renv_tests_scope("breakfast")
  renv::init()

  expected <- c("bread", "breakfast", "oatmeal", "toast")
  lockfile <- renv::snapshot(file = NULL)
  expect_setequal(names(lockfile$R$Package), expected)

})

test_that("we can initialize a project using 'toast'", {

  renv_tests_scope("toast")
  renv::init()

  expected <- c("bread", "toast")
  lockfile <- renv::snapshot(file = NULL)
  expect_setequal(names(lockfile$R$Package), expected)

})
