
test_that("the required Rtools series is inferred from the R version", {
  expect_equal(renv_rtools_required("3.6.3"), "3.5")
  expect_equal(renv_rtools_required("4.0.5"), "4.0")
  expect_equal(renv_rtools_required("4.1.3"), "4.0")
  expect_equal(renv_rtools_required("4.2.1"), "4.2")
  expect_equal(renv_rtools_required("4.5.1"), "4.5")
  expect_equal(renv_rtools_required("4.6.0"), "4.6")
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
