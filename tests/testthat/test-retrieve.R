context("Retrieve")

test_that("We can retrieve packages from git URLs", {
  skip_on_cran()

  record <- list(
    Package   = "renv",
    Source    = "git",
    RemoteUrl = "git://github.com/rstudio/renv.git",
    RemoteSha = "da2656ea344b7b4418d81c97bcc5e092bd4ca7bb"
  )

  records <- list(renv = record)

  renv_restore_begin(records = records, packages = "renv", recursive = FALSE)
  on.exit(renv_restore_end(), add = TRUE)

  result <- renv_retrieve("renv", records)

  record <- result[["renv"]]
  expect_true(record$Version == "0.2.0-27")

  desc <- renv_description_read(record$Path)
  expect_true(desc$RemoteType == "git2r")
  expect_true(desc$RemoteUrl  == "git://github.com/rstudio/renv.git")
  expect_true(desc$RemoteSha  == "da2656ea344b7b4418d81c97bcc5e092bd4ca7bb")

})

test_that("we can retrieve packages from Bitbucket", {
  skip_on_cran()
  skip_if_not_installed("remotes")

  record <- list(
    Package        = "renv",
    Source         = "bitbucket",
    RemoteRepo     = "renv",
    RemoteUsername = "kevinushey",
    RemoteRef      = "master",
    RemoteSha      = "232a3a0132bb59fdd1c29d503be6ce8cf5d3c0fa"
  )

  records <- list(renv = record)

  renv_restore_begin(records = records, packages = "renv", recursive = FALSE)
  on.exit(renv_restore_end(), add = TRUE)

  result <- renv_retrieve("renv", records)

  record <- result[["renv"]]
  expect_true(record$Version == "0.2.0-29")

  desc <- renv_description_read(record$Path)
  expect_true(desc$RemoteType == "bitbucket")
  expect_true(desc$RemoteRepo == "renv")
  expect_true(desc$RemoteSha  == "232a3a0132bb59fdd1c29d503be6ce8cf5d3c0fa")

})
