
test_that("we avoid downloading files twice", {
  skip_on_cran()

  if (!renv_download_method() %in% c("curl", "wget"))
    skip("required downloader not available")

  url <- "https://cloud.r-project.org/src/contrib/Archive/sourcetools/sourcetools_0.1.0.tar.gz"
  destfile <- renv_scope_tempfile("renv-download-", fileext = ".tar.gz")

  # download once and check file metadata
  download(url, destfile, quiet = TRUE)
  before <- renv_file_info(destfile)$mtime

  # download again and check the file info hasn't changed
  download(url, destfile, quiet = TRUE)
  after <- renv_file_info(destfile)$mtime

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

test_that("renv_download_default_agent_scope_impl resets itself", {

  before <- asNamespace("utils")$makeUserAgent
  local(renv_download_default_agent_scope_impl(c("Key" = "Value")))
  expect_equal(asNamespace("utils")$makeUserAgent, before)

})


test_that("we can successfully download files with different downloaders", {
  skip_on_cran()
  skip_on_os("windows")

  # download a small sample file
  url <- "https://cloud.r-project.org/src/base/THANKS"
  destfile <- renv_scope_tempfile("r-thanks-")
  method <- renv_download_method()
  download.file(url, destfile = destfile, quiet = TRUE, method = method)
  thanks <- readLines(destfile)

  if (nzchar(Sys.which("curl"))) local({
    renv_scope_envvars(RENV_DOWNLOAD_FILE_METHOD = "curl")
    destfile <- renv_scope_tempfile("r-curl-thanks-")
    download(url, destfile, quiet = TRUE)
    expect_equal(readLines(destfile), thanks)
  })

  if (renv_platform_windows()) local({
    renv_scope_envvars(RENV_DOWNLOAD_FILE_METHOD = "wininet")
    destfile <- renv_scope_tempfile("r-wininet-thanks-")
    download(url, destfile, quiet = TRUE)
    expect_equal(readLines(destfile), thanks)
  })

  if (capabilities("libcurl") %||% FALSE) local({
    renv_scope_envvars(RENV_DOWNLOAD_FILE_METHOD = "libcurl")
    destfile <- renv_scope_tempfile("r-libcurl-thanks-")
    download(url, destfile, quiet = TRUE)
    expect_equal(readLines(destfile), thanks)
  })

  # TODO: fails on winbuilder
  # if (nzchar(Sys.which("wget"))) local({
  #   renv_scope_envvars(RENV_DOWNLOAD_FILE_METHOD = "wget")
  #   destfile <- renv_scope_tempfile("r-wget-thanks-")
  #   download(url, destfile, quiet = TRUE)
  #   expect_equal(readLines(destfile), thanks)
  # })

})

test_that("downloads work with file URIs", {

  renv_tests_scope()

  repos <- getOption("repos")[["CRAN"]]
  url <- file.path(repos, "src/contrib/PACKAGES")

  destfile <- renv_scope_tempfile("packages-")
  download(url, destfile = destfile)

  expect_true(file.exists(!!destfile))

})

test_that("downloads work with UNC paths on Windows", {
  skip_on_cran()
  skip_if_not(renv_platform_windows())

  renv_tests_scope()

  # get path to repos PACKAGES file
  repos <- getOption("repos")[["CRAN"]]
  base <- sub("^file:/*", "", repos)
  url <- file.path(base, "src/contrib/PACKAGES")
  norm <- renv_path_normalize(url)

  # create server-style path to localhost
  unc <- sub("^([a-zA-Z]):", "//localhost/\\1$", norm)
  expect_true(file.exists(unc))

  destfile <- renv_scope_tempfile("packages-")

  urls <- c(unc, paste0("file:", unc))
  for (url in urls) {
    download(unc, destfile)
    expect_true(file.exists(destfile))
    unlink(destfile)
  }

})

test_that("we can check that a URL is available", {
  skip_on_cran()
  skip_if_not_installed("webfakes")

  app <- webfakes::new_app_process(webfakes::httpbin_app())

  url <- paste0(app$url(), "/bytes/100")
  expect_true(renv_download_available(url))

  # also test the different methods
  expect_true(renv_download_available_headers(url))
  expect_true(renv_download_available_range(url))
  expect_true(renv_download_available_fallback(url))
})

test_that("download failures are reported if destfile not writable", {
  skip_on_cran()
  skip_on_os("windows")

  tdir <- tempfile("renv-forbidden-")
  dir.create(tdir, mode = "0000")
  expect_true(file.exists(tdir))

  tfile <- tempfile("renv-download-", tmpdir = tdir)
  expect_error(
    download(url = "https://cran.r-project.org/", destfile = tfile, quiet = TRUE)
  )

  Sys.chmod(tdir, mode = "0755")
  download(url = "https://cran.r-project.org/", destfile = tfile, quiet = TRUE)
  expect_true(file.exists(tfile))

  unlink(tdir, recursive = TRUE)

})

test_that("renv_curl_version returns a valid numeric_version", {
  skip_if_not(nzchar(Sys.which("curl")))

  version <- renv_curl_version()
  expect_s3_class(version, "numeric_version")
  expect_true(version > "0.0.0")
})

test_that("renv_download_parallel_method respects renv_download_method", {

  # when download method is curl and version supports --parallel
  local({
    renv_scope_envvars(RENV_DOWNLOAD_METHOD = "curl")
    version <- catch(renv_curl_version())
    has_parallel <- !inherits(version, "error") && version >= "7.66.0"
    expected <- if (has_parallel) "curl" else "sequential"
    expect_equal(renv_download_parallel_method(), expected)
  })

  # when download method is libcurl
  local({
    renv_scope_envvars(RENV_DOWNLOAD_METHOD = "libcurl")
    expected <- if (getRversion() >= "4.5.0") "libcurl" else "sequential"
    expect_equal(renv_download_parallel_method(), expected)
  })

  # when download method is something else, always sequential
  local({
    renv_scope_envvars(RENV_DOWNLOAD_METHOD = "wininet")
    expect_equal(renv_download_parallel_method(), "sequential")
  })

})

test_that("renv_download_curl_config_text generates valid config", {

  text <- renv_download_curl_config_text(
    url      = "https://example.com/pkg.tar.gz",
    destfile = "/tmp/pkg.tar.gz",
    type     = NULL,
    headers  = c("X-Custom" = "value")
  )

  expect_true(any(grepl("location", text)))
  expect_true(any(grepl("fail", text)))
  expect_true(any(grepl("silent", text)))
  expect_true(any(grepl("url", text)))
  expect_true(any(grepl("output", text)))
  expect_true(any(grepl("X-Custom: value", text)))

  # HEAD request adds extra flags
  head_text <- renv_download_curl_config_text(
    url      = "https://example.com/pkg.tar.gz",
    destfile = "/tmp/pkg.tar.gz",
    type     = NULL,
    headers  = NULL,
    request  = "HEAD"
  )

  expect_true(any(grepl("head", head_text)))
  expect_true(any(grepl("include", head_text)))

})

test_that("renv_download_parallel works with curl backend", {
  skip_on_cran()
  skip_if_not_installed("webfakes")
  skip_if_not(nzchar(Sys.which("curl")))

  version <- catch(renv_curl_version())
  skip_if(inherits(version, "error") || version < "7.66.0",
          "curl >= 7.66.0 required for --parallel")

  app <- webfakes::new_app_process(webfakes::httpbin_app())

  urls <- paste0(app$url(), "/bytes/", c(100, 200, 300))
  names(urls) <- c("a", "b", "c")
  destfiles <- vapply(names(urls), function(nm) {
    tempfile(paste0("renv-parallel-curl-", nm, "-"))
  }, character(1L))
  types <- rep("unknown", 3L)

  renv_scope_envvars(RENV_DOWNLOAD_METHOD = "curl")
  ok <- renv_download_parallel(urls, destfiles, types)

  expect_true(all(ok))
  expect_equal(names(ok), names(urls))
  for (i in seq_along(destfiles)) {
    expect_true(file.exists(destfiles[[i]]))
    expect_equal(file.size(destfiles[[i]]), c(100, 200, 300)[[i]])
  }

  unlink(destfiles)

})

test_that("renv_download_parallel works with libcurl backend", {
  skip_on_cran()
  skip_if_not_installed("webfakes")
  skip_if_not(getRversion() >= "4.5.0", "R >= 4.5.0 required for vectorized libcurl")
  skip_if_not(capabilities("libcurl"), "libcurl not available")

  app <- webfakes::new_app_process(webfakes::httpbin_app())

  urls <- paste0(app$url(), "/bytes/", c(50, 150))
  names(urls) <- c("x", "y")
  destfiles <- vapply(names(urls), function(nm) {
    tempfile(paste0("renv-parallel-libcurl-", nm, "-"))
  }, character(1L))
  types <- rep("unknown", 2L)

  renv_scope_envvars(RENV_DOWNLOAD_METHOD = "libcurl")
  ok <- renv_download_parallel(urls, destfiles, types)

  expect_true(all(ok))
  expect_equal(names(ok), names(urls))
  for (i in seq_along(destfiles)) {
    expect_true(file.exists(destfiles[[i]]))
    expect_equal(file.size(destfiles[[i]]), c(50, 150)[[i]])
  }

  unlink(destfiles)

})

test_that("renv_download_parallel_sequential works as fallback", {
  skip_on_cran()
  skip_if_not_installed("webfakes")

  app <- webfakes::new_app_process(webfakes::httpbin_app())

  urls <- paste0(app$url(), "/bytes/", c(80, 160))
  names(urls) <- c("p", "q")
  destfiles <- vapply(names(urls), function(nm) {
    tempfile(paste0("renv-parallel-seq-", nm, "-"))
  }, character(1L))
  types <- rep("unknown", 2L)

  ok <- renv_download_parallel_sequential(urls, destfiles, types)

  expect_true(all(ok))
  expect_equal(names(ok), names(urls))
  for (df in destfiles)
    expect_true(file.exists(df))

  unlink(destfiles)

})

test_that("renv_download_parallel reports failure for bad URLs", {
  skip_on_cran()
  skip_if_not_installed("webfakes")

  app <- webfakes::new_app_process(webfakes::httpbin_app())

  urls <- c(
    good = paste0(app$url(), "/bytes/100"),
    bad  = paste0(app$url(), "/status/404")
  )
  destfiles <- vapply(names(urls), function(nm) {
    tempfile(paste0("renv-parallel-fail-", nm, "-"))
  }, character(1L))
  types <- rep("unknown", 2L)

  ok <- renv_download_parallel_sequential(urls, destfiles, types)

  expect_true(ok[["good"]])
  expect_false(ok[["bad"]])

  unlink(destfiles)

})
