
context("Download")

test_that("we avoid downloading files twice", {
  skip_on_cran()

  url <- "https://cran.rstudio.com/src/contrib/Archive/sourcetools/sourcetools_0.1.0.tar.gz"
  destfile <- renv_tempfile("renv-download-", fileext = ".tar.gz")

  # download once and check file metadata
  download(url, destfile)
  info <- file.info(destfile, extra_cols = FALSE)

  # download again and check the file info hasn't changed
  download(url, destfile)
  expect_identical(c(info), c(file.info(destfile, extra_cols = FALSE)))

})
