
context("Retrieve")

test_that("we can retrieve packages from CRAN", {

  skip_on_cran()
  renv_tests_scope()

  record <- list(
    Package = "oatmeal",
    Version = "1.0.0",
    Source  = "CRAN"
  )

  renv_test_retrieve(record)

})

test_that("we can retrieve packages from the CRAN archive", {

  skip_on_cran()
  renv_tests_scope()

  record <- list(
    Package = "bread",
    Version = "0.1.0",
    Source  = "CRAN"
  )

  renv_test_retrieve(record)

})

test_that("packages with an unknown source are retrieved from CRAN", {

  skip_on_cran()
  renv_tests_scope()

  record <- list(
    Package = "bread",
    Version = "0.1.0",
    Source  = "unknown"
  )

  renv_test_retrieve(record)

})

test_that("we can retrieve packages from Bitbucket", {

  skip_on_cran()
  skip("unreliable test")

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

  record <- list(
    Package        = "skeleton",
    Source         = "github",
    RemoteUsername = "kevinushey",
    RemoteRepo     = "skeleton",
    RemoteSha      = "958296dbbbf7f1d82f7f5dd1b121c7558604809f"
  )

  renv_test_retrieve(record)

})

test_that("we can retrieve packages from GitHub (in a sub-directory)", {

  skip_on_cran()

  record <- list(
    Package        = "subdir",
    Source         = "github",
    RemoteUsername = "kevinushey",
    RemoteRepo     = "subdir",
    RemoteSubdir   = "subdir",
    RemoteSha      = "100373b23c8adae1da4e4d6995402d40e9227cfb"
  )

  renv_test_retrieve(record)

})


test_that("we can retrieve packages from GitLab", {

  skip_on_cran()

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
  url <- "https://api.github.com/repos/kevinushey/skeleton/tarball"
  record <- renv_remotes_resolve(url)
  renv_test_retrieve(record)
})

test_that("we can retrieve packages from URL sources", {
  skip_on_cran()

  renv_tests_scope()
  renv_scope_envvars(RENV_PATHS_LOCAL = file.path(getwd(), "local"))

  record <- list(
    Package    = "skeleton",
    Version    = "1.0.1",
    Source     = "URL",
    RemoteType = "url",
    RemoteUrl  = "https://api.github.com/repos/kevinushey/skeleton/tarball"
  )

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

  source <- normalizePath("local/skeleton/skeleton_1.0.1.tar.gz")

  owd <- setwd(tempdir())
  on.exit(setwd(owd), add = TRUE)

  record <- list(
    Package = "skeleton",
    Version = "1.0.1",
    Source  = source
  )

  renv_test_retrieve(record)

})

test_that("explicit path to binary packages work", {

  skip_if_not(renv_platform_macos())

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

test_that("we can retrieve packages from GitHub", {

  skip_on_cran()

  record <- list(
    Package        = "skeleton",
    Source         = "github",
    RemoteHost     = "https://api.github.com",
    RemoteUsername = "kevinushey",
    RemoteRepo     = "skeleton",
    RemoteSha      = "958296dbbbf7f1d82f7f5dd1b121c7558604809f"
  )

  renv_test_retrieve(record)

})

test_that("we can retrieve packages from R repositories", {

  skip_on_cran()
  renv_tests_scope()

  record <- list(
    Package    = "oatmeal",
    Version    = "1.0.0",
    Source     = "Repository",
    Repository = "CRAN"
  )

  renv_test_retrieve(record)

})

test_that("we can retrieve files using file URIs", {
  skip_on_cran()
  renv_tests_scope()

  source <- file.path(getwd(), "source")
  target <- file.path(getwd(), "target")

  writeLines("Hello, world!", con = source)

  # plain 'file:' URI with no authority
  uri <- paste("file:", source, sep = "")
  download(uri, destfile = target)
  expect_equal(readLines(target), "Hello, world!")
  unlink(target)

  # file URI using empty authority
  prefix <- if (renv_platform_windows()) "file:///" else "file://"
  uri <- paste(prefix, source, sep = "")
  download(uri, destfile = target)
  expect_equal(readLines(target), "Hello, world!")
  unlink(target)

})
