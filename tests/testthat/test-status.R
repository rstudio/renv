test_that("reports if status not possible", {

  renv_tests_scope()
  expect_snapshot(status())

  init(bare = TRUE)
  expect_snapshot(status())

  snapshot()
  unlink("renv/library", recursive = TRUE)
  expect_snapshot(status())

})

test_that("reports when project is synchronised", {

  renv_tests_scope()
  init()

  expect_snapshot(status())

})

test_that("reports installation problems with non-installed packages", {

  renv_tests_scope()
  init()

  writeLines(c("library(egg)", "library(bread)"), con = "script.R")
  record("egg")
  record("oatmeal")

  expect_snapshot(status())

})

test_that("reports synchronisation problems with installed packages", {

  renv_tests_scope()
  init()
  install(c("egg", "bread"))

  writeLines("library(bread)", con = "script.R")
  record("egg")

  expect_snapshot(status())

})

test_that("reports version differences", {

  renv_tests_scope(c("egg", "oatmeal"))
  init()
  record("egg@2.0.0")
  record("oatmeal@0.9.0")

  expect_snapshot(status())

})
