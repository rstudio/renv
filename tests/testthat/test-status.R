test_that("not installed/recorded/used", {

  renv_tests_scope("bread")
  init()
  snapshot()
  remove("bread")

  expect_snapshot(status())

})

test_that("installed/not recorded/used", {

  renv_tests_scope()
  init()
  install("bread")
  writeLines("library(bread)", con = "script.R")

  expect_snapshot(status())

})

test_that("not installed/*/used", {

  renv_tests_scope()
  init()
  writeLines("library(bread)", con = "script.R")
  expect_snapshot(status())

  record("bread")
  expect_snapshot(status())

})

test_that("*/recorded/not used", {

  renv_tests_scope()
  init()
  record("egg")

  expect_snapshot(status())

})

test_that("installed/*/not used", {

  renv_tests_scope()
  init()
  install("egg")

  expect_snapshot(status())

})

test_that("other changes", {

  renv_tests_scope(c("egg", "oatmeal"))
  init()
  record("egg@2.0.0")
  record("oatmeal@0.9.0")

  expect_snapshot(status())

})
