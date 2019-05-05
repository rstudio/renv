
context("Download")

test_that("we avoid downloading files twice", {
  skip_on_cran()
  skip_if_offline()

  if (!renv_download_file_method() %in% c("curl", "wget"))
    skip("required downloader not available")

  url <- "https://cran.rstudio.com/src/contrib/Archive/sourcetools/sourcetools_0.1.0.tar.gz"
  destfile <- renv_tempfile("renv-download-", fileext = ".tar.gz")

  # download once and check file metadata
  download(url, destfile)
  before <- file.info(destfile, extra_cols = FALSE)

  # download again and check the file info hasn't changed
  download(url, destfile)
  after <- file.info(destfile, extra_cols = FALSE)

  # check that they're the same.
  expect_identical(c(before), c(after))

})

test_that("we can successfully tweak the user agent string", {

  utils <- asNamespace("utils")

  ok <-
    is.function(utils$makeUserAgent) &&
    identical(formals(utils$makeUserAgent), pairlist(format = TRUE))

  if (!ok)
    return(NULL)

  headers <- c("Key" = "Value")

  before <- utils$makeUserAgent
  expect_true(renv_download_default_agent_scope_impl(headers = headers))
  after <- utils$makeUserAgent

  expect_identical(
    paste0(before(format = TRUE), "Key: Value\r\n"),
    after(format = TRUE)
  )

  expect_identical(before(format = FALSE), after(format = FALSE))

})
