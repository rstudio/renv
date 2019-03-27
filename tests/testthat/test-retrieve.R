
context("Retrieve")

# TODO: all these tests rely on having private access tokens
# set up for authentication with some private repositories;
# should see if i can provide scoped keys for use here

test_that("we can retrieve packages from Bitbucket", {
  skip_on_cran()
  skip_if_not_installed("remotes")

  record <- list(
    Package        = "skeleton",
    Source         = "bitbucket",
    RemoteRepo     = "skeleton",
    RemoteUsername = "kevinushey",
    RemoteRef      = "master",
    RemoteSha      = "209c4e48e505e545ad7ab915904d983b5ab83b93"
  )

  records <- list(skeleton = record)

  renv_restore_begin(records = records, packages = "skeleton", recursive = FALSE)
  on.exit(renv_restore_end(), add = TRUE)

  result <- renv_retrieve("skeleton", records)

  record <- result[["skeleton"]]
  expect_true(record$Version == "1.0.0")

  desc <- renv_description_read(record$Path)
  expect_true(desc$RemoteType == "bitbucket")
  expect_true(desc$RemoteRepo == "skeleton")
  expect_true(desc$RemoteSha  == "209c4e48e505e545ad7ab915904d983b5ab83b93")

})


test_that("we can retrieve packages from git", {
  skip_on_cran()
  skip_if_not_installed("remotes")

  record <- list(
    Package   = "skeleton",
    Source    = "git",
    RemoteUrl = "git://github.com/kevinushey/skeleton.git",
    RemoteSha = "209c4e48e505e545ad7ab915904d983b5ab83b93"
  )

  records <- list(skeleton = record)

  renv_restore_begin(records = records, packages = "skeleton", recursive = FALSE)
  on.exit(renv_restore_end(), add = TRUE)

  result <- renv_retrieve("skeleton", records)

  record <- result[["skeleton"]]
  expect_true(record$Version == "1.0.0")

  desc <- renv_description_read(record$Path)
  expect_true(desc$RemoteType == "git")
  expect_true(desc$RemoteUrl  == "git://github.com/kevinushey/skeleton.git")
  expect_true(desc$RemoteSha  == "209c4e48e505e545ad7ab915904d983b5ab83b93")

})


test_that("we can retrieve packages from GitHub", {
  skip_on_cran()
  skip_if_not_installed("remotes")

  record <- list(
    Package        = "skeleton",
    Source         = "github",
    RemoteUsername = "kevinushey",
    RemoteRepo     = "skeleton",
    RemoteSha      = "209c4e48e505e545ad7ab915904d983b5ab83b93"
  )

  records <- list(skeleton = record)

  renv_restore_begin(records = records, packages = "skeleton", recursive = FALSE)
  on.exit(renv_restore_end(), add = TRUE)

  result <- renv_retrieve("skeleton", records)

  record <- result[["skeleton"]]
  expect_true(record$Version == "1.0.0")

  desc <- renv_description_read(record$Path)
  expect_true(desc$RemoteType == "github")
  expect_true(desc$RemoteRepo == "skeleton")
  expect_true(desc$RemoteSha  == "209c4e48e505e545ad7ab915904d983b5ab83b93")

})


test_that("we can retrieve packages from GitLab", {
  skip_on_cran()
  skip_if_not_installed("remotes")

  record <- list(
    Package        = "skeleton",
    Source         = "gitlab",
    RemoteUsername = "kevinushey",
    RemoteRepo     = "skeleton",
    RemoteSha      = "209c4e48e505e545ad7ab915904d983b5ab83b93"
  )

  records <- list(skeleton = record)

  renv_restore_begin(records = records, packages = "skeleton", recursive = FALSE)
  on.exit(renv_restore_end(), add = TRUE)

  result <- renv_retrieve("skeleton", records)

  record <- result[["skeleton"]]
  expect_true(record$Version == "1.0.0")

  desc <- renv_description_read(record$Path)
  expect_true(desc$RemoteType == "gitlab")
  expect_true(desc$RemoteRepo == "skeleton")
  expect_true(desc$RemoteSha  == "209c4e48e505e545ad7ab915904d983b5ab83b93")

})


