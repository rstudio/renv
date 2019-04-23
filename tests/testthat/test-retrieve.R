
context("Retrieve")

test_that("we can retrieve packages from Bitbucket", {
  skip_on_cran()

  # TODO
  skip_on_travis()

  record <- list(
    Package        = "skeleton",
    Source         = "bitbucket",
    RemoteRepo     = "skeleton",
    RemoteUsername = "kevinushey",
    RemoteSha      = "958296dbbbf7f1d82f7f5dd1b121c7558604809f"
  )

  renv_test_retrieve(record)

})


test_that("we can retrieve packages from git", {
  skip_on_cran()
  skip_if_not_installed("remotes")

  # TODO
  skip_on_travis()

  record <- list(
    Package   = "skeleton",
    Source    = "git",
    RemoteUrl = "git://github.com/kevinushey/skeleton.git",
    RemoteSha = "958296dbbbf7f1d82f7f5dd1b121c7558604809f"
  )

  renv_test_retrieve(record)

})


test_that("we can retrieve packages from GitHub", {
  skip_on_cran()

  # TODO
  skip_on_travis()

  record <- list(
    Package        = "skeleton",
    Source         = "github",
    RemoteUsername = "kevinushey",
    RemoteRepo     = "skeleton",
    RemoteSha      = "958296dbbbf7f1d82f7f5dd1b121c7558604809f"
  )

  renv_test_retrieve(record)

})


test_that("we can retrieve packages from GitLab", {
  skip_on_cran()

  # TODO
  skip_on_travis()

  record <- list(
    Package        = "skeleton",
    Source         = "gitlab",
    RemoteUsername = "kevinushey",
    RemoteRepo     = "skeleton",
    RemoteSha      = "958296dbbbf7f1d82f7f5dd1b121c7558604809f"
  )

  renv_test_retrieve(record)

})

test_that("we can retrieve packages with URLs", {
  skip_on_cran()
  skip_on_travis()
  record <- renv_remotes_parse("https://api.github.com/repos/kevinushey/skeleton/tarball")
  renv_test_retrieve(record)
})

test_that("we can retrieve packages from local sources", {

  renv_scope_envvars(RENV_PATHS_LOCAL = file.path(getwd(), "local"))

  record <- list(
    Package = "skeleton",
    Version = "1.0.1",
    Source  = "local"
  )

  renv_test_retrieve(record)

})
