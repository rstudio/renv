context("Init")

test_that("we can initialize a project using 'breakfast'", {

  callback <- renv_tests_scope("breakfast")
  on.exit(callback(), add = TRUE)

  renv::init()

  expected <- c("bread", "breakfast", "oatmeal", "toast")
  lockfile <- renv::snapshot(lockfile = NULL)
  expect_setequal(names(lockfile$R$Package), expected)

})

test_that("we can initialize a project using 'toast'", {

  callback <- renv_tests_scope("toast")
  on.exit(callback(), add = TRUE)

  renv::init()

  expected <- c("bread", "toast")
  lockfile <- renv::snapshot(lockfile = NULL)
  expect_setequal(names(lockfile$R$Package), expected)

})

test_that("we cannot initialize a project using 'brunch'", {

  callback <- renv_tests_scope("brunch")
  on.exit(callback(), add = TRUE)

  # 'brunch' will fail to install
  renv::init()

  expect_false(file.exists(renv_paths_library("brunch")))

})

test_that("attempts to initialize a project with a missing package is okay", {

  callback <- renv_tests_scope("missing")
  on.exit(callback(), add = TRUE)

  # package 'missing' does not exist and so cannot be installed
  renv::init()

  expect_false(file.exists(renv_paths_library("missing")))

})
