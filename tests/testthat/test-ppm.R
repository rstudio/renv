
test_that("we can transform binary URLs into source URLs", {
  skip_on_cran()
  skip_on_os("windows")

  url <- "https://packagemanager.rstudio.com/cran/__linux__/centos7/latest"

  actual <- renv_ppm_normalize(url)
  expected <- "https://packagemanager.rstudio.com/cran/latest"
  expect_identical(actual, expected)

})

test_that("repository URLs are properly transformed for different platforms", {
  skip_on_cran()
  skip_on_os("windows")

  renv_scope_envvars(RENV_RSPM_OS = "__linux__", RENV_RSPM_PLATFORM = "bionic")
  repos <- c(RSPM = "https://cluster.rstudiopm.com/cran/latest")
  expected <- c(RSPM = "https://cluster.rstudiopm.com/cran/__linux__/bionic/latest")
  actual <- renv_ppm_transform(repos)
  expect_identical(expected, actual)

})

test_that("a binary-specific URL is transformed before writing a lockfile", {
  skip_on_cran()

  renv_tests_scope()
  renv_scope_options(repos = c(RSPM = "https://cluster.rstudiopm.com/cran/__os__/platform/latest"))
  lockfile <- snapshot(lockfile = NULL)
  actual <- convert(lockfile$R$Repositories, "character")
  expected <- c(RSPM = "https://cluster.rstudiopm.com/cran/latest")
  expect_identical(actual, expected)

})

test_that("RSPM bits are preserved when writing lockfile", {
  skip_on_cran()

  renv_tests_scope()
  renv_scope_options(repos = c(RSPM = "https://cluster.rstudiopm.com/curated/__linux__/bionic/12"))
  lockfile <- snapshot(lockfile = NULL)
  actual <- convert(lockfile$R$Repositories, "character")
  expected <- c(RSPM = "https://cluster.rstudiopm.com/curated/12")
  expect_identical(actual, expected)

})

test_that("RSPM is confirmed not supported on trusty", {
  skip_on_cran()
  renv_scope_envvars(RENV_RSPM_OS = "__linux__", RENV_RSPM_PLATFORM = "trusty")
  before <- "https://cluster.rstudiopm.com/cran/latest"
  after  <- renv_ppm_transform(before)
  expect_identical(unname(before), unname(after))
})

test_that("renv correctly detects RHEL as CentOS for RSPM", {
  skip_on_cran()
  skip_on_os("windows")

  release <- heredoc('
    NAME="Red Hat Enterprise Linux Server"
    VERSION="7.9 (Maipo)"
    ID="rhel"
    ID_LIKE="fedora"
    VARIANT="Server"
    VARIANT_ID="server"
    VERSION_ID="7.9"
    PRETTY_NAME="Red Hat Enterprise Linux Server 7.9 (Maipo)"
    ANSI_COLOR="0;31"
    CPE_NAME="cpe:/o:redhat:enterprise_linux:7.9:GA:server"
    HOME_URL="https://www.redhat.com/"
    BUG_REPORT_URL="https://bugzilla.redhat.com/"
  ')

  file <- renv_scope_tempfile()
  writeLines(release, con = file)

  platform <- renv_ppm_platform_impl(file = file)
  expect_equal(platform, "centos7")

})

test_that("URLs like http://foo/bar aren't queried", {
  skip_on_cran()
  skip_on_os("windows")

  # pretend to be Ubuntu
  renv_scope_envvars(
    RENV_PPM_OS = "__linux__",
    RENV_PPM_PLATFORM = "ubuntu"
  )

  # emit a condition that we can catch on
  renv_scope_trace(renv:::renv_ppm_status, quote(
    renv_condition_signal("bail")
  ))

  # should exit early
  status <- expect_no_condition(renv_ppm_transform_impl("http://foo/bar"), class = "bail")
  expect_equal(status, "http://foo/bar")

  # should emit a condition now
  expect_condition(renv_ppm_transform_impl("http://foo/bar/baz"), class = "bail")

})

test_that("renv_ppm_transform() uses source URLs when appropriate", {
  skip_on_cran()
  skip_on_os("windows")

  # pretend to be Ubuntu
  renv_scope_envvars(
    RENV_PPM_OS = "__linux__",
    RENV_PPM_PLATFORM = "jammy"
  )

  binurl <- "https://packagemanager.rstudio.com/cran/__linux__/jammy/latest"
  srcurl <- "https://packagemanager.rstudio.com/cran/latest"

  # prefer source URLs
  renv_scope_binding(the, "install_pkg_type", "source")
  expect_equal(unname(renv_ppm_transform(binurl)), srcurl)
  expect_equal(unname(renv_ppm_transform(srcurl)), srcurl)

  # prefer binary URLs
  renv_scope_binding(the, "install_pkg_type", "binary")
  expect_equal(unname(renv_ppm_transform(binurl)), binurl)
  expect_equal(unname(renv_ppm_transform(srcurl)), binurl)

})
