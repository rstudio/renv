
test_that("/etc/os-release files can be parsed", {

  contents <- '
# This is a comment.

NAME="Ubuntu"
VERSION="18.04.5 LTS (Bionic Beaver)"
ID=ubuntu
ID_LIKE=debian
PRETTY_NAME="Ubuntu 18.04.5 LTS"
VERSION_ID="18.04"
HOME_URL="https://www.ubuntu.com/"
SUPPORT_URL="https://help.ubuntu.com/"
BUG_REPORT_URL="https://bugs.launchpad.net/ubuntu/"
PRIVACY_POLICY_URL="https://www.ubuntu.com/legal/terms-and-policies/privacy-policy"
VERSION_CODENAME=bionic
UBUNTU_CODENAME=bionic
'

  file <- renv_scope_tempfile("os-release-")
  writeLines(contents, con = file)

  sysinfo <- list(sysname = "linux")
  prefix <- renv_bootstrap_platform_os_via_os_release(file, sysinfo)
  expect_equal(prefix, "linux-ubuntu-bionic")

})

test_that("the platform record describes the running system", {

  project <- renv_tests_scope()
  record <- renv_platform_record(project = project)

  expect_equal(record$R$Version, format(getRversion()))
  expect_equal(record$R$Platform, R.version$platform)
  expect_equal(record$OS$Name, Sys.info()[["sysname"]])
  expect_type(record$Locale$Collate, "character")

  if (renv_platform_linux())
    expect_true(is.null(record$OS$Distro) || is.character(record$OS$Distro))
  else
    expect_null(record$OS$Distro)

  if (!renv_platform_windows())
    expect_null(record$Rtools)

  # the record can be written as JSON, and read back
  path <- renv_scope_tempfile("renv-platform-", fileext = ".json")
  renv_json_write(record, file = path)
  expect_equal(names(renv_json_read(path)), names(record))

})

test_that("system package versions can be queried", {

  expect_equal(renv_sysreqs_versions("deb", character()), list())

  skip_if_not(nzchar(Sys.which("dpkg-query")))
  versions <- renv_sysreqs_versions("deb", c("dpkg", "no-such-package-renv"))
  expect_equal(names(versions), "dpkg")
  expect_true(nzchar(versions$dpkg))

})
