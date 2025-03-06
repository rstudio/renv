
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

test_that("renv correctly detects RHEL < 9 as CentOS for RSPM", {
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

test_that("renv correctly detects RHEL9 for PPM", {
  skip_on_cran()
  skip_on_os("windows")

  release <- heredoc('
    NAME="Red Hat Enterprise Linux"
    VERSION="9.0 (Plow)"
    ID="rhel"
    ID_LIKE="fedora"
    VERSION_ID="9.0"
    PLATFORM_ID="platform:el9"
    PRETTY_NAME="Red Hat Enterprise Linux 9.0 (Plow)"
    ANSI_COLOR="0;31"
    LOGO="fedora-logo-icon"
    CPE_NAME="cpe:/o:redhat:enterprise_linux:9::baseos"
    HOME_URL="https://www.redhat.com/"
    DOCUMENTATION_URL="https://access.redhat.com/documentation/red_hat_enterprise_linux/9/"
    BUG_REPORT_URL="https://bugzilla.redhat.com/"

    REDHAT_BUGZILLA_PRODUCT="Red Hat Enterprise Linux 9"
    REDHAT_BUGZILLA_PRODUCT_VERSION=9.0
    REDHAT_SUPPORT_PRODUCT="Red Hat Enterprise Linux"
    REDHAT_SUPPORT_PRODUCT_VERSION="9.0"
  ')

  file <- renv_scope_tempfile()
  writeLines(release, con = file)

  platform <- renv_ppm_platform_impl(file = file)
  expect_equal(platform, "rhel9")

})

test_that("renv correctly detects Rocky Linux 8 as centos8 for PPM", {
  skip_on_cran()
  skip_on_os("windows")

  release <- heredoc('
    NAME="Rocky Linux"
    VERSION="8.8 (Green Obsidian)"
    ID="rocky"
    ID_LIKE="rhel centos fedora"
    VERSION_ID="8.8"
    PLATFORM_ID="platform:el8"
    PRETTY_NAME="Rocky Linux 8.8 (Green Obsidian)"
    ANSI_COLOR="0;32"
    LOGO="fedora-logo-icon"
    CPE_NAME="cpe:/o:rocky:rocky:8:GA"
    HOME_URL="https://rockylinux.org/"
    BUG_REPORT_URL="https://bugs.rockylinux.org/"
    SUPPORT_END="2029-05-31"
    ROCKY_SUPPORT_PRODUCT="Rocky-Linux-8"
    ROCKY_SUPPORT_PRODUCT_VERSION="8.8"
    REDHAT_SUPPORT_PRODUCT="Rocky Linux"
    REDHAT_SUPPORT_PRODUCT_VERSION="8.8"
  ')

  file <- renv_scope_tempfile()
  writeLines(release, con = file)

  platform <- renv_ppm_platform_impl(file = file)
  expect_equal(platform, "centos8")

})

test_that("renv correctly detects Rocky Linux 9 as rhel9 for PPM", {
  skip_on_cran()
  skip_on_os("windows")

  release <- heredoc('
    NAME="Rocky Linux"
    VERSION="9.2 (Blue Onyx)"
    ID="rocky"
    ID_LIKE="rhel centos fedora"
    VERSION_ID="9.2"
    PLATFORM_ID="platform:el9"
    PRETTY_NAME="Rocky Linux 9.2 (Blue Onyx)"
    ANSI_COLOR="0;32"
    LOGO="fedora-logo-icon"
    CPE_NAME="cpe:/o:rocky:rocky:9::baseos"
    HOME_URL="https://rockylinux.org/"
    BUG_REPORT_URL="https://bugs.rockylinux.org/"
    SUPPORT_END="2032-05-31"
    ROCKY_SUPPORT_PRODUCT="Rocky-Linux-9"
    ROCKY_SUPPORT_PRODUCT_VERSION="9.2"
    REDHAT_SUPPORT_PRODUCT="Rocky Linux"
    REDHAT_SUPPORT_PRODUCT_VERSION="9.2"
  ')

  file <- renv_scope_tempfile()
  writeLines(release, con = file)

  platform <- renv_ppm_platform_impl(file = file)
  expect_equal(platform, "rhel9")

})

test_that("renv correctly detects AlmaLinux 9 as rhel9 for PPM", {
  skip_on_cran()
  skip_on_os("windows")

  release <- heredoc('
    NAME="AlmaLinux"
    VERSION="9.2 (Turquoise Kodkod)"
    ID="almalinux"
    ID_LIKE="rhel centos fedora"
    VERSION_ID="9.2"
    PLATFORM_ID="platform:el9"
    PRETTY_NAME="AlmaLinux 9.2 (Turquoise Kodkod)"
    ANSI_COLOR="0;34"
    LOGO="fedora-logo-icon"
    CPE_NAME="cpe:/o:almalinux:almalinux:9::baseos"
    HOME_URL="https://almalinux.org/"
    DOCUMENTATION_URL="https://wiki.almalinux.org/"
    BUG_REPORT_URL="https://bugs.almalinux.org/"

    ALMALINUX_MANTISBT_PROJECT="AlmaLinux-9"
    ALMALINUX_MANTISBT_PROJECT_VERSION="9.2"
    REDHAT_SUPPORT_PRODUCT="AlmaLinux"
    REDHAT_SUPPORT_PRODUCT_VERSION="9.2"
  ')

  file <- renv_scope_tempfile()
  writeLines(release, con = file)

  platform <- renv_ppm_platform_impl(file = file)
  expect_equal(platform, "rhel9")

})

test_that("renv correctly detects OpenSUSE for PPM", {
  skip_on_cran()
  skip_on_os("windows")

  release <- heredoc('
    NAME="openSUSE Leap"
    VERSION="15.4"
    ID="opensuse-leap"
    ID_LIKE="suse opensuse"
    VERSION_ID="15.4"
    PRETTY_NAME="openSUSE Leap 15.4"
    ANSI_COLOR="0;32"
    CPE_NAME="cpe:/o:opensuse:leap:15.4"
    BUG_REPORT_URL="https://bugs.opensuse.org"
    HOME_URL="https://www.opensuse.org/"
    DOCUMENTATION_URL="https://en.opensuse.org/Portal:Leap"
    LOGO="distributor-logo-Leap"
  ')

  file <- renv_scope_tempfile()
  writeLines(release, con = file)

  platform <- renv_ppm_platform_impl(file = file)
  expect_equal(platform, "opensuse154")

})

test_that("renv correctly detects SLES as OpenSUSE for PPM", {
  skip_on_cran()
  skip_on_os("windows")

  # `docker run -it registry.suse.com/suse/sle15:15.5`
  release <- heredoc('
    NAME="SLES"
    VERSION="15-SP5"
    VERSION_ID="15.5"
    PRETTY_NAME="SUSE Linux Enterprise Server 15 SP5"
    ID="sles"
    ID_LIKE="suse"
    ANSI_COLOR="0;32"
    CPE_NAME="cpe:/o:suse:sles:15:sp5"
    DOCUMENTATION_URL="https://documentation.suse.com/"
  ')

  file <- renv_scope_tempfile()
  writeLines(release, con = file)

  platform <- renv_ppm_platform_impl(file = file)
  expect_equal(platform, "opensuse155")

})

test_that("renv correctly detects Debian for PPM", {
  skip_on_cran()
  skip_on_os("windows")

  release <- heredoc('
    PRETTY_NAME="Debian GNU/Linux 11 (bullseye)"
    NAME="Debian GNU/Linux"
    VERSION_ID="11"
    VERSION="11 (bullseye)"
    VERSION_CODENAME=bullseye
    ID=debian
    HOME_URL="https://www.debian.org/"
    SUPPORT_URL="https://www.debian.org/support"
    BUG_REPORT_URL="https://bugs.debian.org/"
  ')

  file <- renv_scope_tempfile()
  writeLines(release, con = file)

  platform <- renv_ppm_platform_impl(file = file)
  expect_equal(platform, "bullseye")

})

test_that("renv correctly detects Ubuntu for PPM", {
  skip_on_cran()
  skip_on_os("windows")

  release <- heredoc('
    NAME="Ubuntu"
    VERSION="20.04.6 LTS (Focal Fossa)"
    ID=ubuntu
    ID_LIKE=debian
    PRETTY_NAME="Ubuntu 20.04.6 LTS"
    VERSION_ID="20.04"
    HOME_URL="https://www.ubuntu.com/"
    SUPPORT_URL="https://help.ubuntu.com/"
    BUG_REPORT_URL="https://bugs.launchpad.net/ubuntu/"
    PRIVACY_POLICY_URL="https://www.ubuntu.com/legal/terms-and-policies/privacy-policy"
    VERSION_CODENAME=focal
    UBUNTU_CODENAME=focal
  ')

  file <- renv_scope_tempfile()
  writeLines(release, con = file)

  platform <- renv_ppm_platform_impl(file = file)
  expect_equal(platform, "focal")

})

test_that("renv correctly detects Amazon Linux 2 as centos7 for PPM", {
  skip_on_cran()
  skip_on_os("windows")

  release <- heredoc('
    NAME="Amazon Linux"
    VERSION="2"
    ID="amzn"
    ID_LIKE="centos rhel fedora"
    VERSION_ID="2"
    PRETTY_NAME="Amazon Linux 2"
    ANSI_COLOR="0;33"
    CPE_NAME="cpe:2.3:o:amazon:amazon_linux:2"
    HOME_URL="https://amazonlinux.com/"
    SUPPORT_END="2025-06-30"
  ')

  file <- renv_scope_tempfile()
  writeLines(release, con = file)

  platform <- renv_ppm_platform_impl(file = file)
  expect_equal(platform, "centos7")

})

test_that("renv detects no supported PPM platform for Amazon Linux 2023", {
  skip_on_cran()
  skip_on_os("windows")

  release <- heredoc('
    NAME="Amazon Linux"
    VERSION="2023"
    ID="amzn"
    ID_LIKE="fedora"
    VERSION_ID="2023"
    PLATFORM_ID="platform:al2023"
    PRETTY_NAME="Amazon Linux 2023"
    ANSI_COLOR="0;33"
    CPE_NAME="cpe:2.3:o:amazon:amazon_linux:2023"
    HOME_URL="https://aws.amazon.com/linux/"
    BUG_REPORT_URL="https://github.com/amazonlinux/amazon-linux-2023"
    SUPPORT_END="2028-03-01"
  ')

  file <- renv_scope_tempfile()
  writeLines(release, con = file)

  platform <- renv_ppm_platform_impl(file = file)
  expect_null(platform)

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
