
context("Download")

test_that("we avoid downloading files twice", {
  skip_on_cran()

  url <- "https://cran.rstudio.com/src/contrib/Archive/sourcetools/sourcetools_0.1.0.tar.gz"
  destfile <- renv_tempfile()

  output <- stack()
  withCallingHandlers({
    download(url, destfile)
    download(url, destfile)
  }, message = function(m) {
    output$push(conditionMessage(m))
    invokeRestart("muffleMessage")
  })

  expect_true(any(grepl("file is up to date", output$data())))

})
