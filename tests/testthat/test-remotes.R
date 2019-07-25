
context("Remotes")

test_that("we can parse a variety of remotes", {
  skip_on_cran()

  renv_tests_scope()

  # cran latest
  record <- renv_remotes_parse("breakfast")
  expect_equal(record$Package, "breakfast")
  expect_equal(record$Version, "1.0.0")

  # cran archive
  record <- renv_remotes_parse("breakfast@0.1.0")
  expect_equal(record$Package, "breakfast")
  expect_equal(record$Version, "0.1.0")

  # github master
  record <- renv_remotes_parse("kevinushey/skeleton")
  expect_equal(record$Package, "skeleton")
  expect_equal(record$Version, "1.0.1")

  # by commit
  record <- renv_remotes_parse("kevinushey/skeleton@209c4e48e505e545ad7ab915904d983b5ab83b93")
  expect_equal(record$Package, "skeleton")
  expect_equal(record$Version, "1.0.0")

  # by branch
  record <- renv_remotes_parse("kevinushey/skeleton@feature/version-bump")
  expect_equal(record$Package, "skeleton")
  expect_equal(record$Version, "1.0.2")

  # by PR
  record <- renv_remotes_parse("kevinushey/skeleton#1")
  expect_equal(record$Package, "skeleton")
  expect_equal(record$Version, "1.0.2")

})
