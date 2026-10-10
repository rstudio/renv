
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

test_that("the toolchain preflight is a no-op outside of Windows", {
  skip_on_os("windows")
  records <- list(bread = list(Package = "bread", Version = "1.0.0", Source = "Repository"))
  expect_true(renv_rtools_preflight(records))
})

test_that("records which can be installed without a toolchain are not reported", {

  renv_tests_scope()

  # pretend only source packages are available from the repositories;
  # the test repositories only have binaries on some platforms
  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_graph_url_repository_types",
    replacement = function() "source"
  )

  # the test packages have no compiled code (write_PACKAGES() records
  # NeedsCompilation: no for them), so they don't need a toolchain
  bread <- list(Package = "bread", Version = "1.0.0", Source = "Repository")
  expect_false(renv_rtools_preflight_required(bread))

  # packages not in the repositories at all are assumed to need one
  missing <- list(Package = "nosuchpackage", Version = "1.0.0", Source = "Repository")
  expect_true(renv_rtools_preflight_required(missing))

  # Bioconductor repositories aren't available yet during preflight
  bioc <- list(Package = "Biobase", Version = "2.60.0", Source = "Bioconductor")
  expect_false(renv_rtools_preflight_required(bioc))

  # packages whose current release has compiled code do need one
  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_available_packages_entry",
    replacement = function(package, type = "source", ...) {
      if (identical(type, "binary"))
        stop("no binaries available")
      list(Package = package, NeedsCompilation = "yes")
    }
  )
  expect_true(renv_rtools_preflight_required(bread))

})

test_that("binaries available from the repositories are not reported", {

  renv_tests_scope()

  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_graph_url_repository_types",
    replacement = function() c("binary", "source")
  )

  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_available_packages_entry",
    replacement = function(package, type = "source", ...) {
      list(Package = package, NeedsCompilation = "yes")
    }
  )

  bread <- list(Package = "bread", Version = "1.0.0", Source = "Repository")
  expect_false(renv_rtools_preflight_required(bread))

})

test_that("the toolchain preflight reports packages needing a source build", {

  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_rtools_preflight_required",
    replacement = function(record) record$Package %in% c("ragg", "stringi")
  )

  records <- list(
    stringi = list(Package = "stringi", Version = "1.8.4", Source = "Repository"),
    cli     = list(Package = "cli", Version = "3.6.3", Source = "Repository"),
    ragg    = list(Package = "ragg", Version = "1.3.0", Source = "Repository")
  )

  expect_snapshot(status <- renv_rtools_preflight_impl(records, version = "4.5.1"))
  expect_false(status)

  expect_true(renv_rtools_preflight_impl(records["cli"], version = "4.5.1"))

})
