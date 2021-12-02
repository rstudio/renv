
context("Remotes")

test_that("we can parse a variety of remotes", {

  remote <- renv_remotes_parse("git@github.com:kevinushey/repo.git:subdir")

  expect_equal(remote$type,   "git")
  expect_equal(remote$host,   "github.com")
  expect_equal(remote$user,   "kevinushey")
  expect_equal(remote$repo,   "repo")
  expect_equal(remote$ext,    "git")
  expect_equal(remote$subdir, "subdir")

  remote <- renv_remotes_parse("url::https://github.com/kevinushey/renv.git1/archive/refs/heads/main.zip")

  expect_equal(remote$type,     "url")
  expect_equal(remote$url,      "https://github.com/kevinushey/renv.git1/archive/refs/heads/main.zip")
  expect_equal(remote$protocol, "https")

})

test_that("we can parse a variety of remotes", {
  skip_on_cran()
  skip_on_os("windows")

  renv_tests_scope()

  # cran latest
  record <- renv_remotes_resolve("breakfast")
  expect_equal(record$Package, "breakfast")
  expect_equal(record$Version, NULL)

  # cran archive
  record <- renv_remotes_resolve("breakfast@0.1.0")
  expect_equal(record$Package, "breakfast")
  expect_equal(record$Version, "0.1.0")

  # github master
  record <- renv_remotes_resolve("kevinushey/skeleton")
  expect_equal(record$Package, "skeleton")
  expect_equal(record$Version, "1.0.1")
  expect_equal(record$RemoteRef, "master")
  expect_equal(record$RemoteSha, "e4aafb92b86ba7eba3b7036d9d96fdfb6c32761a")

  # by commit
  record <- renv_remotes_resolve("kevinushey/skeleton@209c4e48e505e545ad7ab915904d983b5ab83b93")
  expect_equal(record$Package, "skeleton")
  expect_equal(record$Version, "1.0.0")
  expect_equal(record$RemoteSha, "209c4e48e505e545ad7ab915904d983b5ab83b93")

  # by branch
  record <- renv_remotes_resolve("kevinushey/skeleton@feature/version-bump")
  expect_equal(record$Package, "skeleton")
  expect_equal(record$Version, "1.0.2")
  expect_equal(record$RemoteRef, "feature/version-bump")
  expect_equal(record$RemoteSha, "86b5737411d3c6a6927dfcccd2c15a69284659fe")

  # by PR
  record <- renv_remotes_resolve("kevinushey/skeleton#1")
  expect_equal(record$Package, "skeleton")
  expect_equal(record$Version, "1.0.2")
  expect_equal(record$RemoteSha, "86b5737411d3c6a6927dfcccd2c15a69284659fe")

  # bitbucket
  record <- renv_remotes_resolve("bitbucket::kevinushey/skeleton")
  expect_equal(record$Package, "skeleton")
  expect_equal(record$Version, "1.0.1")
  expect_equal(record$RemoteHost, "api.bitbucket.org/2.0")
  expect_equal(record$RemoteRef, "master")
  expect_equal(record$RemoteSha, "958296dbbbf7f1d82f7f5dd1b121c7558604809f")

  # gitlab
  record <- renv_remotes_resolve("gitlab::kevinushey/skeleton")
  expect_equal(record$Package, "skeleton")
  expect_equal(record$Version, "1.0.1")
  expect_equal(record$RemoteHost, "gitlab.com")
  expect_equal(record$RemoteRef, "master")
  expect_equal(record$RemoteSha, "958296dbbbf7f1d82f7f5dd1b121c7558604809f")

  # git - https
  record <- renv_remotes_resolve("git::https://github.com/kevinushey/renv.git1.git@main")
  expect_equal(record$Package, "renv.git1")
  expect_equal(record$Version, "0.0.0.9000")
  expect_equal(record$RemoteUrl, "https://github.com/kevinushey/renv.git1.git")
  expect_equal(record$RemoteRef, "main")

  # url
  record <- renv_remotes_resolve("url::https://github.com/kevinushey/renv.git1/archive/refs/heads/main.zip")
  expect_equal(record$Package, "renv.git1")
  expect_equal(record$Version, "0.0.0.9000")
  expect_equal(record$RemoteUrl, "https://github.com/kevinushey/renv.git1/archive/refs/heads/main.zip")
  expect_equal(record$Source, "URL")

  # # git - ssh
  # # this test appears to fail on CI (ssh access to GitHub disallowed?)
  # record <- renv_remotes_resolve("git::git@github.com:kevinushey/renv.git1.git@main")
  # expect_equal(record$Package, "renv.git1")
  # expect_equal(record$Version, "0.0.0.9000")
  # expect_equal(record$RemoteUrl, "git@github.com:kevinushey/renv.git1.git")
  # expect_equal(record$RemoteRef, "main")

  # error
  expect_error(renv_remotes_resolve("can't parse this"))

})

test_that("subdirectories are parsed in remotes", {

  spec <- "gitlab::user/repo:subdir@ref"
  remote <- renv_remotes_parse(spec)

  expected <- list(
    spec   = spec,
    type   = "gitlab",
    host   = NULL,
    user   = "user",
    repo   = "repo",
    subdir = "subdir",
    pull   = NULL,
    ref    = "ref"
  )

  expect_equal(remote, expected)

})

test_that("custom hosts can be supplied", {

  spec <- "gitlab@localhost::user/repo"
  remote <- renv_remotes_parse(spec)

  expected <- list(
    spec   = spec,
    type   = "gitlab",
    host   = "localhost",
    user   = "user",
    repo   = "repo",
    subdir = NULL,
    pull   = NULL,
    ref    = NULL
  )

  expect_equal(remote, expected)

})

test_that("paths specified with '.' are treated as local", {

  renv_tests_scope()

  writeLines(con = "DESCRIPTION", c(
    "Type: Package",
    "Package: test",
    "Version: 1.0"
  ))

  record <- renv_remotes_resolve(".")
  expect_equal(record$Package, "test")
  expect_equal(record$Version, "1.0")

})

test_that("packages can be installed from GitLab groups", {

  # test parsing of spec
  spec <- "gitlab::renv-group/renv-subgroup/subpackage"
  remote <- renv_remotes_parse(spec)

  expected <- list(
    spec   = spec,
    type   = "gitlab",
    host   = NULL,
    user   = "renv-group",
    repo   = "renv-subgroup/subpackage",
    subdir = NULL,
    pull   = NULL,
    ref    = NULL
  )

  expect_equal(remote, expected)

  # test installation
  skip_sometimes()
  renv_tests_scope()
  renv::install(spec)
  expect_true(renv_package_installed("subpackage"))

})

test_that("remote specs referencing packages in sub-sub-directories are parsed correctly", {

  spec <- "github::user/repo/subdir/subsubdir"
  remote <- renv_remotes_parse(spec)

  expected <- list(
    spec   = spec,
    type   = "github",
    host   = NULL,
    user   = "user",
    repo   = "repo",
    subdir = "subdir/subsubdir",
    pull   = NULL,
    ref    = NULL
  )

  expect_equal(remote, expected)

})
