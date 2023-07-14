
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
