
context("Remotes")

test_that("we can parse a variety of remotes", {
  skip_on_cran()
  skip_on_appveyor()

  renv_tests_scope()

  # cran latest
  record <- renv_remotes_resolve("breakfast")
  expect_equal(record$Package, "breakfast")
  expect_equal(record$Version, "1.0.0")

  # cran archive
  record <- renv_remotes_resolve("breakfast@0.1.0")
  expect_equal(record$Package, "breakfast")
  expect_equal(record$Version, "0.1.0")

  # github master
  record <- renv_remotes_resolve("kevinushey/skeleton")
  expect_equal(record$Package, "skeleton")
  expect_equal(record$Version, "1.0.1")

  # by commit
  record <- renv_remotes_resolve("kevinushey/skeleton@209c4e48e505e545ad7ab915904d983b5ab83b93")
  expect_equal(record$Package, "skeleton")
  expect_equal(record$Version, "1.0.0")

  # by branch
  record <- renv_remotes_resolve("kevinushey/skeleton@feature/version-bump")
  expect_equal(record$Package, "skeleton")
  expect_equal(record$Version, "1.0.2")

  # by PR
  record <- renv_remotes_resolve("kevinushey/skeleton#1")
  expect_equal(record$Package, "skeleton")
  expect_equal(record$Version, "1.0.2")

})

test_that("subdirectories are parsed in remotes", {

  entry <- "gitlab::user/repo:subdir@ref"
  parsed <- renv_remotes_parse(entry)

  expected <- list(
    entry  = entry,
    type   = "gitlab",
    host   = "",
    user   = "user",
    repo   = "repo",
    subdir = "subdir",
    pull   = "",
    ref    = "ref"
  )

  expect_equal(parsed, expected)

})

test_that("custom hosts can be supplied", {

  entry <- "gitlab@localhost:user/repo"
  parsed <- renv_remotes_parse(entry)

  expected <- list(
    entry  = entry,
    type   = "gitlab",
    host   = "localhost",
    user   = "user",
    repo   = "repo",
    subdir = "",
    pull   = "",
    ref    = ""
  )

  expect_equal(parsed, expected)

})
