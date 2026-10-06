
test_that("package types are classified as R classifies them", {

  expect_equal(renv_pkgtype_class("source"), "source")
  expect_equal(renv_pkgtype_class("win.binary"), "win.binary")
  expect_equal(renv_pkgtype_class("mac.binary"), "mac.binary")
  expect_equal(renv_pkgtype_class("mac.binary.big-sur-arm64"), "mac.binary")

  # custom binary types introduced with R 4.6.0
  expect_equal(renv_pkgtype_class("macos.binary.arm64"), "other.binary")
  expect_equal(renv_pkgtype_class("windows.binary.clang-aarch64"), "other.binary")
  expect_equal(renv_pkgtype_class("linux.binary"), "other.binary")

  # the virtual "binary" and "both" types resolve to the current build's type
  expect_equal(renv_pkgtype_class("binary"), renv_pkgtype_class(.Platform$pkgType))
  expect_equal(renv_pkgtype_class("both"), renv_pkgtype_class(.Platform$pkgType))

})

test_that("package types map to the archive extensions R expects", {

  expect_equal(renv_pkgtype_ext("source"), ".tar.gz")
  expect_equal(renv_pkgtype_ext("win.binary"), ".zip")
  expect_equal(renv_pkgtype_ext("mac.binary.big-sur-arm64"), ".tgz")
  expect_equal(renv_pkgtype_ext("macos.binary.arm64"), ".tar.xz")
  expect_equal(renv_pkgtype_ext("linux.binary"), ".tar.xz")

})

test_that("build designations match those written by R CMD INSTALL --build", {

  expect_equal(renv_pkgtype_build("macos.binary.arm64"), "macos-arm64")
  expect_equal(renv_pkgtype_build("windows.binary.clang-aarch64"), "windows-clang-aarch64")
  expect_equal(renv_pkgtype_build("linux.binary"), "linux")

  # legacy binary types carry no build designation in their file names
  expect_null(renv_pkgtype_build("win.binary"))
  expect_null(renv_pkgtype_build("mac.binary.big-sur-arm64"))

})
