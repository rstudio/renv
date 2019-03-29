
context("Download")

test_that("we avoid downloading files twice", {
  skip_on_cran()

  url <- "https://cran.rstudio.com/src/contrib/Archive/sourcetools/sourcetools_0.1.0.tar.gz"
  destfile <- renv_tempfile()

  output <- capture.output({
    download(url, destfile)
    download(url, destfile)
  })

  expect_true(any(grepl("file is up to date", output)))

})
