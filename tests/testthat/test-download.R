
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

  if (renv_platform_windows()) local({
    renv_scope_envvars(RENV_DOWNLOAD_FILE_METHOD = "wininet")
    destfile <- tempfile("r-wininet-thanks-")
    download(url, destfile, quiet = TRUE)
    expect_equal(readLines(destfile), thanks)
  })

  if (capabilities("libcurl") %||% FALSE) local({
    renv_scope_envvars(RENV_DOWNLOAD_FILE_METHOD = "libcurl")
    destfile <- tempfile("r-libcurl-thanks-")
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

})

# https://github.com/rstudio/renv/issues/390
test_that("downloads still succeed even if HEAD request fails", {
  skip_on_cran()

  url <- "https://github.com/kevinushey/skeleton/releases/download/v1.0.1/skeleton_1.0.1.tar.gz"
  destfile <- file.path(tempdir(), basename(url))
  on.exit(unlink(destfile), add = TRUE)

  # HEAD requests will fail on GitHub releases
  expect_warning(
    renv_download_headers(
      url     = url,
      type    = "github",
      headers = list()
    )
  )

  # but a plain old download should be fine
  download(url, destfile)

  # validate the md5sum
  md5 <- tools::md5sum(destfile)
  expect_equal(unname(md5), "9e978ca14adea985850f7913fc837166")

})

test_that("downloads work with file URIs", {

  renv_tests_scope()

  repos <- getOption("repos")[["CRAN"]]
  url <- file.path(repos, "src/contrib/PACKAGES")

  destfile <- tempfile("packages-")
  download(url, destfile = destfile)

  expect_true(file.exists(destfile))

})


test_that("downloads work with UNC paths on Windows", {
  skip_on_cran()
  skip_if_not(renv_platform_windows())

  renv_tests_scope()

  # get path to repos PACKAGES file
  repos <- getOption("repos")[["CRAN"]]
  base <- sub("^file:/*", "", repos)
  url <- file.path(base, "src/contrib/PACKAGES")
  norm <- normalizePath(url, winslash = "/")

  # create server-style path to localhost
  unc <- sub("^([a-zA-Z]):", "//localhost/\\1$", norm)
  expect_true(file.exists(unc))

  destfile <- renv_tempfile("packages-")

  urls <- c(unc, paste0("file:", unc))
  for (url in urls) {
    download(unc, destfile)
    expect_true(file.exists(destfile))
    unlink(destfile)
  }

})
