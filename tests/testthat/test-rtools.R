
test_that("the required Rtools series is inferred from the R version", {

  expect_equal(renv_rtools_required("3.6.3"), "3.5")
  expect_equal(renv_rtools_required("4.0.5"), "4.0")
  expect_equal(renv_rtools_required("4.1.3"), "4.0")
  expect_equal(renv_rtools_required("4.2.1"), "4.2")
  expect_equal(renv_rtools_required("4.5.1"), "4.5")

  # the newest series serves newer releases of R until the next appears
  expect_equal(renv_rtools_required("4.6.0"), "4.5")
  series <- c(renv_rtools_series(), "4.6")
  expect_equal(renv_rtools_required("4.6.0", series), "4.6")
  expect_equal(renv_rtools_required("4.5.1", series), "4.5")

  # a series newer than R is never appropriate, so a list missing the older
  # series falls back to the known ones
  expect_equal(renv_rtools_required("4.1.3", c("4.2", "4.3")), "4.0")
  expect_equal(renv_rtools_required("4.3.2", c("4.4", "4.5")), "4.3")

})

test_that("Rtools installations are discovered and matched to R", {

  # pretend the system drive is a temporary directory, so that the default
  # installation locations can be populated
  drive <- renv_scope_tempfile("renv-drive-")
  ensure_directory(file.path(drive, c("rtools45", "rtools46")))
  renv_scope_envvars(
    SYSTEMDRIVE   = drive,
    RTOOLS45_HOME = NULL,
    RTOOLS44_HOME = NULL,
    RTOOLS43_HOME = NULL,
    RTOOLS42_HOME = NULL,
    RTOOLS40_HOME = NULL
  )

  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_rtools_registry",
    replacement = function() NULL
  )

  # both installations are found, even though only one has a dedicated entry
  specs <- renv_rtools_list()
  specs <- filter(specs, function(spec) startsWith(spec$root, drive))
  expect_setequal(map_chr(specs, `[[`, "version"), c("4.5", "4.6"))

  # ignore any Rtools installations actually present on this machine
  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_rtools_list",
    replacement = function() specs
  )

  # the newest compatible series is preferred
  expect_equal(renv_rtools_find(rversion = "4.5.1")$version, "4.5")
  expect_equal(renv_rtools_find(rversion = "4.6.0")$version, "4.6")
  expect_null(renv_rtools_find(rversion = "4.2.1"))

})

test_that("Rtools compatibility follows the series' supported R versions", {

  spec <- function(version) list(version = version)

  expect_true(renv_rtools_compatible(spec("4.2"), rversion = "4.2.1"))
  expect_false(renv_rtools_compatible(spec("4.2"), rversion = "4.3.0"))
  expect_true(renv_rtools_compatible(spec("4.5"), rversion = "4.6.0"))
  expect_false(renv_rtools_compatible(spec("4.6"), rversion = "4.5.1"))
  expect_true(renv_rtools_compatible(spec("4.6"), rversion = "4.7.0"))
  expect_false(renv_rtools_compatible(spec(NULL), rversion = "4.5.1"))

})

test_that("the available Rtools series can be read from the CRAN index page", {

  html <- c(
    '<a href="rtools45/rtools.html">RTools 4.5</a>',
    '<a href="rtools44/rtools.html">RTools 4.4</a>',
    '<a href="rtools42/rtools.html">RTools 4.2</a>',
    '<a href="rtools40.html">RTools 4.0</a>',
    '<a href="history.html">old versions</a>'
  )

  expect_equal(renv_rtools_series_parse(html), c("4.0", "4.2", "4.4", "4.5"))
  expect_equal(renv_rtools_series_parse(character()), character())

})

test_that("the Rtools installer URL is read from the series page", {

  expect_equal(
    renv_rtools_page_url("4.0"),
    "https://cran.r-project.org/bin/windows/Rtools/rtools40.html"
  )

  expect_equal(
    renv_rtools_page_url("4.5"),
    "https://cran.r-project.org/bin/windows/Rtools/rtools45/rtools.html"
  )

  html <- c(
    '<p><a href="files/rtools45-6768-6492.exe">Rtools45 installer</a></p>',
    '<p><a href="files/rtools45-aarch64-6768-6492.exe">Rtools45 aarch64 installer</a></p>'
  )

  expect_equal(
    renv_rtools_installer_url_parse(html, "4.5", arch = "x86_64"),
    "files/rtools45-6768-6492.exe"
  )

  expect_equal(
    renv_rtools_installer_url_parse(html, "4.5", arch = "aarch64"),
    "files/rtools45-aarch64-6768-6492.exe"
  )

  expect_null(renv_rtools_installer_url_parse(html, "4.5", arch = "i386"))

  # installers for other series are ignored
  expect_null(renv_rtools_installer_url_parse(html, "4.4", arch = "x86_64"))

  html <- c(
    '<a href="rtools40-x86_64.exe">rtools40-x86_64.exe</a>',
    '<a href="rtools40-i686.exe">rtools40-i686.exe</a>'
  )

  expect_equal(
    renv_rtools_installer_url_parse(html, "4.0", arch = "x86_64"),
    "rtools40-x86_64.exe"
  )

  expect_equal(
    renv_rtools_installer_url_parse(html, "4.0", arch = "i386"),
    "rtools40-i686.exe"
  )

})
