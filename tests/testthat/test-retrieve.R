
test_that("we can retrieve packages from CRAN", {

  skip_slow()

  renv_tests_scope()

  record <- list(
    Package = "oatmeal",
    Version = "1.0.0",
    Source  = "CRAN"
  )

  renv_test_retrieve(record)

})

test_that("we can retrieve packages from the CRAN archive", {

  skip_slow()

  renv_tests_scope()

  record <- list(
    Package = "bread",
    Version = "0.1.0",
    Source  = "CRAN"
  )

  renv_test_retrieve(record)

})

test_that("packages with an unknown source are retrieved from CRAN", {

  skip_slow()

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
  skip("unreliable test")

  record <- list(
    Package   = "skeleton",
    Source    = "git",
    RemoteUrl = "git://github.com/kevinushey/skeleton.git",
    RemoteSha = "958296dbbbf7f1d82f7f5dd1b121c7558604809f"
  )

  renv_test_retrieve(record)

})

test_that("we can retrieve packages with git dependencies", {
  skip_on_cran()
  skip_slow()

  # GitHub doesn't like ssh (used as remote field in renv.git1)
  skip_on_ci()

  record <- list(
    Package   = "renv.git1",
    Source    = "git",
    RemoteUrl = "https://github.com/kevinushey/renv.git1",
    RemoteRef = "main"
  )

  renv_test_retrieve(record)
})


test_that("we can retrieve packages from GitHub", {

  skip_slow()

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

  skip_slow()

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

  skip_slow()

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
  skip_slow()
  url <- "https://api.github.com/repos/kevinushey/skeleton/tarball"
  record <- renv_remotes_resolve(url)
  renv_test_retrieve(record)
})

test_that("we can retrieve packages from URL sources", {

  skip_slow()

  renv_tests_scope()
  renv_scope_local()

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

  renv_tests_scope()
  renv_scope_local()

  record <- list(
    Package = "skeleton",
    Version = "1.0.1",
    Source  = "local"
  )

  renv_test_retrieve(record)

})

test_that("compatible local sources are preferred when available", {

  renv_tests_scope()
  renv_scope_local()

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

  renv_tests_scope()
  renv_scope_local()

  source <- renv_tests_path("local/skeleton/skeleton_1.0.1.tar.gz")
  renv_scope_wd(tempdir())

  record <- list(
    Package = "skeleton",
    Version = "1.0.1",
    Source  = source
  )

  renv_test_retrieve(record)

})



test_that("explicit path to binary packages work", {

  skip_if_not(renv_platform_macos())

  renv_tests_scope()
  renv_scope_local()

  # make sure we have a binary package in the cellar to test with
  srcpath <- renv_tests_path("local/skeleton/skeleton_1.0.1.tar.gz")
  binpath <- renv_tests_path("local/skeleton/skeleton_1.0.1.tgz")
  defer(unlink(binpath))

  # create the binary
  local({
    renv_scope_wd(dirname(srcpath))
    args <- c("CMD", "INSTALL", "--build", basename(srcpath))
    renv_system_exec(R(), args)
  })

  record <- list(
    Package = "skeleton",
    Version = "1.0.1",
    Source  = binpath
  )

  renv_test_retrieve(record)

})

test_that("remotes::install_local() records are handled", {

  renv_scope_envvars(RENV_PATHS_LOCAL = "")

  record <- list(
    Package    = "skeleton",
    Version    = "1.0.1",
    Source     = "local",
    RemoteUrl  = renv_tests_path("local/skeleton/skeleton_1.0.1.tar.gz")
  )

  renv_test_retrieve(record)

})

test_that("we can retrieve packages from GitHub", {

  skip_slow()

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

test_that("records with RemoteSha successfully retrieved from archives", {

  renv_tests_scope()

  record <- list(
    Package   = "bread",
    Version   = "0.1.0",
    Source    = "Repository",
    RemoteSha = "oops"
  )

  renv_test_retrieve(record)

})

test_that("we respect the default branch for gitlab repositories", {
  skip_on_cran()
  remote <- renv_remotes_resolve("gitlab::kevinushey/main")
  expect_equal(remote$RemoteRef, "main")
})

test_that("renv can retrieve the latest release associated with a project", {
  skip_if_no_github_auth()

  remote <- renv_remotes_resolve("rstudio/keras@*release")
  expect_true(is.list(remote))
})

test_that("retrieve handles local sources", {
  skip_on_cran()

  renv_tests_scope()

  record <- list(
    Package   = "bread",
    Version   = "1.0.0",
    Source    = "bread_1.0.0.tar.gz"
  )

  expect_error(renv_test_retrieve(record))

  # call download.packages once to get URL
  url <- download.packages("bread", destdir = getwd())
  if (!file.exists(record$Source))
    file.copy(url[1, 2], record$Source)

  renv_test_retrieve(record)

})
