
test_that("the way each record will be installed is classified", {

  renv_tests_scope()

  # earlier tests may have installed the test packages into the cache,
  # in which case no build would be needed
  renv_scope_envvars(RENV_PATHS_CACHE = renv_scope_tempfile("renv-cache-"))

  # pretend only source packages are available from the repositories;
  # the test repositories only have binaries on some platforms
  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_graph_url_repository_types",
    replacement = function() "source"
  )

  # the test packages have no compiled code (write_PACKAGES() records
  # NeedsCompilation: no for them)
  bread <- list(Package = "bread", Version = "1.0.0", Source = "Repository")
  expect_equal(renv_install_preflight_binaries_type(bread), "source")

  # packages not in the repositories at all can't be classified
  missing <- list(Package = "nosuchpackage", Version = "1.0.0", Source = "Repository")
  expect_equal(renv_install_preflight_binaries_type(missing), "unknown")

  # Bioconductor repositories aren't available yet during preflight
  bioc <- list(Package = "Biobase", Version = "2.60.0", Source = "Bioconductor")
  expect_equal(renv_install_preflight_binaries_type(bioc), "none")

  # packages whose current release has compiled code need compiling
  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_available_packages_entry",
    replacement = function(package, type = "source", ...) {
      if (identical(type, "binary"))
        stop("no binaries available")
      list(Package = package, NeedsCompilation = "yes")
    }
  )
  expect_equal(renv_install_preflight_binaries_type(bread), "compiled")

})

test_that("binaries available from the repositories need no build", {

  renv_tests_scope()
  renv_scope_envvars(RENV_PATHS_CACHE = renv_scope_tempfile("renv-cache-"))

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
  expect_equal(renv_install_preflight_binaries_type(bread), "none")

})

test_that("packages to be compiled from source are reported", {

  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_install_preflight_binaries_type",
    replacement = function(record) {
      switch(
        record$Package,
        ragg    = "compiled",
        stringi = "compiled",
        cli     = "none",
        rlang   = "source",
        mypkg   = "unknown"
      )
    }
  )

  records <- list(
    stringi = list(Package = "stringi", Version = "1.8.4", Source = "Repository"),
    cli     = list(Package = "cli", Version = "3.6.3", Source = "Repository"),
    rlang   = list(Package = "rlang", Version = "1.1.4", Source = "Repository"),
    mypkg   = list(Package = "mypkg", Version = "0.1.0", Source = "GitHub"),
    ragg    = list(Package = "ragg", Version = "1.3.0", Source = "Repository")
  )

  # with a toolchain, this is informational
  expect_snapshot(status <- renv_install_preflight_binaries_impl(records, rtools = TRUE))
  expect_true(status)

  # without one, the install is expected to fail
  expect_snapshot({
    status <- renv_install_preflight_binaries_impl(
      records,
      rtools  = FALSE,
      version = "4.5.1"
    )
  })
  expect_false(status)

  # nothing is reported when nothing needs compiling
  expect_silent(status <- renv_install_preflight_binaries_impl(records["cli"]))
  expect_true(status)

})
