
context("Retrieve")

test_that("we can retrieve packages from Bitbucket", {
  skip_on_cran()
  skip_on_travis()
  skip_if_offline(host = "bitbucket.org")

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
  skip_on_travis()
  skip_if_offline(host = "github.com")

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
  skip_on_travis()
  skip_if_offline(host = "github.com")

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
  skip_on_travis()
  skip_if_offline(host = "gitlab.com")

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
  skip_if_offline(host = "github.com")
  url <- "https://api.github.com/repos/kevinushey/skeleton/tarball"
  record <- renv_remotes_parse(url)
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

test_that("compatible local sources are preferred when available", {

  renv_scope_envvars(RENV_PATHS_LOCAL = file.path(getwd(), "local"))

  record <- list(
    Package = "skeleton",
    Version = "1.0.1",
    Source  = "CRAN"
  )

  renv_test_retrieve(record)

  record <- list(
    Package = "skeleton",
    Version = "1.0.1",
    Source  = "unknown"
  )

  renv_test_retrieve(record)

})

test_that("an explicitly-provided local source path can be used", {

  record <- list(
    Package = "skeleton",
    Version = "1.0.1",
    Source  = "local/skeleton/skeleton_1.0.1.tar.gz"
  )

  renv_test_retrieve(record)

  record <- list(
    Package = "skeleton",
    Version = "1.0.1",
    Source  = "local/skeleton/skeleton_1.0.1.tgz"
  )

  renv_test_retrieve(record)

})

test_that("remotes::install_local() records are handled", {

  record <- list(
    Package    = "skeleton",
    Version    = "1.0.1",
    Source     = "local",
    RemoteUrl  = "local/skeleton/skeleton_1.0.1.tar.gz"
  )

  renv_test_retrieve(record)

})
