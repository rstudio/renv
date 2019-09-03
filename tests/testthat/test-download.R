
context("Download")

test_that("we avoid downloading files twice", {
  skip_on_cran()

  if (!renv_download_file_method() %in% c("curl", "wget"))
    skip("required downloader not available")

  url <- "https://cloud.r-project.org/src/contrib/Archive/sourcetools/sourcetools_0.1.0.tar.gz"
  destfile <- renv_tempfile("renv-download-", fileext = ".tar.gz")

  # download once and check file metadata
  download(url, destfile, quiet = TRUE)
  before <- file.info(destfile, extra_cols = FALSE)$mtime

  # download again and check the file info hasn't changed
  download(url, destfile, quiet = TRUE)
  after <- file.info(destfile, extra_cols = FALSE)$mtime

  # check that they're the same.
  expect_identical(before, after)

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

test_that("we can successfully download files with different downloaders", {
  skip_on_cran()
  skip_on_appveyor()

  # download a small sample file
  url <- "https://cloud.r-project.org/src/base/THANKS"
  destfile <- tempfile("r-thanks-")
  method <- renv_download_file_method()
  download.file(url, destfile = destfile, quiet = TRUE, method = method)
  thanks <- readLines(destfile)

  if (nzchar(Sys.which("curl"))) local({
    renv_scope_envvars(RENV_DOWNLOAD_FILE_METHOD = "curl")
    destfile <- tempfile("r-curl-thanks-")
    download(url, destfile, quiet = TRUE)
    expect_equal(readLines(destfile), thanks)
  })

  # TODO: fails on winbuilder
  # if (nzchar(Sys.which("wget"))) local({
  #   renv_scope_envvars(RENV_DOWNLOAD_FILE_METHOD = "wget")
  #   destfile <- tempfile("r-wget-thanks-")
  #   download(url, destfile, quiet = TRUE)
  #   expect_equal(readLines(destfile), thanks)
  # })

  if (getRversion() >= "3.3.0") local({
    renv_scope_envvars(RENV_DOWNLOAD_FILE_METHOD = "internal")
    destfile <- tempfile("r-internal-thanks-")
    download(url, destfile, quiet = TRUE)
    expect_equal(readLines(destfile), thanks)
  })

})
