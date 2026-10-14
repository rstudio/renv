
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

test_that("only installed system packages are reported", {

  # dpkg reports on packages which were removed but not purged
  output <- c(
    "libcurl4-openssl-dev 7.81.0-1ubuntu1 config-files",
    "libssl-dev 3.0.2-0ubuntu1 installed",
    "zlib1g-dev 1:1.2.11.dfsg-2ubuntu9 installed",
    "dpkg-query: no packages found matching no-such-package"
  )

  packages <- c("libcurl4-openssl-dev", "libssl-dev", "no-such-package")
  versions <- renv_sysreqs_versions_parse(output, packages)
  expect_equal(versions, list("libssl-dev" = "3.0.2-0ubuntu1"))

})

test_that("the external software record tolerates a missing extSoftVersion()", {

  expect_null(renv_platform_record_blas(NULL))
  expect_null(renv_platform_record_software(NULL))

  software <- c(BLAS = "libblas.so", zlib = "1.2.11", bzlib = "")
  expect_equal(renv_platform_record_blas(software), "libblas.so")
  expect_equal(renv_platform_record_software(software), list(zlib = "1.2.11"))

})

test_that("tool versions are read from the first meaningful line of output", {

  expect_equal(renv_platform_record_tool_version("pandoc 3.1.11"), "pandoc 3.1.11")
  expect_null(renv_platform_record_tool_version(character()))

  # the JVM announces options from the environment before its version
  output <- c(
    "Picked up JAVA_TOOL_OPTIONS: -Xmx2g",
    "openjdk version \"17.0.2\" 2022-01-18",
    "OpenJDK Runtime Environment (build 17.0.2+8)"
  )

  expect_equal(
    renv_platform_record_tool_version(output),
    "openjdk version \"17.0.2\" 2022-01-18"
  )

})

test_that("external tools are reported when available", {

  expect_null(renv_platform_record_tool("", "--version"))
  expect_null(renv_platform_record_tool(file.path(tempdir(), "nosuchtool"), "--version"))

  pandoc <- renv_platform_record_tool_pandoc()
  skip_if_not(nzchar(pandoc))

  tool <- renv_platform_record_tool(pandoc, "--version")
  expect_match(tool$Version, "pandoc")
  expect_true(file.exists(tool$Path))

  tools <- renv_platform_record_tools()
  expect_equal(tools$Pandoc, tool)

})
