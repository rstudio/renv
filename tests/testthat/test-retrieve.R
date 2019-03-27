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

  renv_restore_begin(records = records, packages = "digest", recursive = FALSE)
  on.exit(renv_restore_end(), add = TRUE)

  result <- renv_retrieve("digest", list(digest = record))

  record <- result[["renv"]]
  expect_true(record$Version == "0.2.0-27")

  desc <- renv_description_read(record$Path)
  expect_true(desc$RemoteType == "git")
  expect_true(desc$RemoteUrl  == "git://github.com/rstudio/renv.git")
  expect_true(desc$RemoteSha  == "da2656ea344b7b4418d81c97bcc5e092bd4ca7bb")

})
