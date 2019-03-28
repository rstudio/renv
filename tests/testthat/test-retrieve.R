
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
    RemoteSha      = "209c4e48e505e545ad7ab915904d983b5ab83b93"
  )

  renv_test_retrieve(record)

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

  renv_test_retrieve(record)

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

  renv_test_retrieve(record)

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

  renv_test_retrieve(record)

})


