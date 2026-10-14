
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

  # a description resolved by the dependency graph already says whether the
  # package needs compiling, so the repositories aren't consulted again
  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_available_packages_entry",
    replacement = function(...) stop("should not be called")
  )
  bread$NeedsCompilation <- "no"
  expect_equal(renv_install_preflight_binaries_type(bread), "source")

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
    replacement = function(record, project = NULL) {
      switch(
        record$Package,
        ragg    = "compiled",
        stringi = "compiled",
        cli     = "none",
        rlang   = "source",
        mypkg   = "unknown",
        broken  = "compiled"
      )
    }
  )

  records <- list(
    stringi = list(Package = "stringi", Version = "1.8.4", Source = "Repository"),
    cli     = list(Package = "cli", Version = "3.6.3", Source = "Repository"),
    rlang   = list(Package = "rlang", Version = "1.1.4", Source = "Repository"),
    mypkg   = list(Package = "mypkg", Version = "0.1.0", Source = "GitHub"),
    ragg    = list(Package = "ragg", Version = "1.3.0", Source = "Repository"),
    broken  = structure(list(Package = "broken"), resolution_failed = TRUE)
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

  # packages which might need compiling are only a concern without a toolchain
  expect_silent(status <- renv_install_preflight_binaries_impl(records["mypkg"]))
  expect_true(status)

  expect_snapshot({
    status <- renv_install_preflight_binaries_impl(
      records["mypkg"],
      rtools  = FALSE,
      version = "4.5.1"
    )
  })
  expect_false(status)

})

# pretend the requested version of a package is only available from the
# archive, and that the current release has the given need for compilation
renv_test_preflight_scope_archived <- function(needs, scope = parent.frame()) {

  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_graph_url_repository_types",
    replacement = function() c("binary", "source"),
    scope       = scope
  )

  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_available_packages_entry",
    replacement = function(package, type = "source", filter = NULL, ...) {
      if (identical(type, "binary") || !is.null(filter))
        stop("version not available")
      list(Package = package, NeedsCompilation = needs)
    },
    scope       = scope
  )

}

test_that("P3M lookup failures don't abort the preflight check", {

  renv_tests_scope()
  renv_scope_envvars(RENV_PATHS_CACHE = renv_scope_tempfile("renv-cache-"))
  renv_test_preflight_scope_archived(needs = "no")

  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_p3m_enabled",
    replacement = function(type = NULL) TRUE
  )

  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_retrieve_repos_p3m_date",
    replacement = function(record) stop("could not download the P3M database")
  )

  bread <- list(Package = "bread", Version = "1.0.0", Source = "Repository")
  expect_equal(renv_install_preflight_binaries_type(bread), "source")

})

test_that("P3M binaries aren't counted for records pinned to a repository", {

  renv_tests_scope()
  renv_scope_envvars(RENV_PATHS_CACHE = renv_scope_tempfile("renv-cache-"))
  renv_test_preflight_scope_archived(needs = "yes")

  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_p3m_enabled",
    replacement = function(type = NULL) TRUE
  )

  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_retrieve_repos_p3m_date",
    replacement = function(record) as.Date("2024-01-15")
  )

  bread <- list(
    Package    = "bread",
    Version    = "1.0.0",
    Source     = "Repository",
    Repository = getOption("repos")[[1L]]
  )

  expect_equal(renv_install_preflight_binaries_type(bread), "none")

  renv_scope_restore(project = getwd(), strict = TRUE)
  expect_equal(renv_install_preflight_binaries_type(bread), "compiled")

})

test_that("a source-only install doesn't count P3M binaries", {

  renv_tests_scope()
  renv_scope_envvars(RENV_PATHS_CACHE = renv_scope_tempfile("renv-cache-"))
  renv_test_preflight_scope_archived(needs = "yes")

  # P3M would normally be consulted for this session
  renv_scope_options(pkgType = "both", renv.config.ppm.enabled = TRUE)

  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_retrieve_repos_p3m_date",
    replacement = function(record) as.Date("2024-01-15")
  )

  bread <- list(Package = "bread", Version = "1.0.0", Source = "Repository")
  expect_equal(renv_install_preflight_binaries_type(bread), "none")

  # but the type requested by install() governs, as it does for the installer
  renv_scope_binding(the, "install_pkg_type", "source")
  expect_equal(renv_install_preflight_binaries_type(bread), "compiled")

})

test_that("P3M binaries aren't counted for versions the repositories serve", {

  renv_tests_scope()
  renv_scope_envvars(RENV_PATHS_CACHE = renv_scope_tempfile("renv-cache-"))

  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_graph_url_repository_types",
    replacement = function() c("binary", "source")
  )

  # the requested version is served as source only; the installer builds
  # it from there rather than looking to P3M
  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_available_packages_entry",
    replacement = function(package, type = "source", ...) {
      if (identical(type, "binary"))
        stop("no binaries available")
      list(Package = package, NeedsCompilation = "yes")
    }
  )

  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_p3m_enabled",
    replacement = function(type = NULL) TRUE
  )

  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_retrieve_repos_p3m_date",
    replacement = function(record) stop("should not be called")
  )

  bread <- list(Package = "bread", Version = "1.0.0", Source = "Repository")
  expect_equal(renv_install_preflight_binaries_type(bread), "compiled")

})

test_that("strict restores only look for binaries in the recorded repository", {

  renv_tests_scope()
  renv_scope_envvars(RENV_PATHS_CACHE = renv_scope_tempfile("renv-cache-"))

  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_graph_url_repository_types",
    replacement = function() c("binary", "source")
  )

  # another configured repository has a binary; the recorded one does not
  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_available_packages_entry",
    replacement = function(package, type = "source", repos = NULL, ...) {
      if (identical(type, "binary") && !is.null(repos))
        stop("no binaries in the recorded repository")
      list(Package = package, NeedsCompilation = "yes")
    }
  )

  bread <- list(
    Package    = "bread",
    Version    = "1.0.0",
    Source     = "Repository",
    Repository = getOption("repos")[[1L]]
  )

  expect_equal(renv_install_preflight_binaries_type(bread), "none")

  renv_scope_restore(project = getwd(), strict = TRUE)
  expect_equal(renv_install_preflight_binaries_type(bread), "compiled")

})

# pretend a built copy of every package exists in the cache
renv_test_preflight_scope_cache <- function(scope = parent.frame()) {

  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_cache_find",
    replacement = function(record) file.path(tempdir(), record$Package),
    scope       = scope
  )

  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_cache_package_validate",
    replacement = function(path) TRUE,
    scope       = scope
  )

  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_graph_url_repository_types",
    replacement = function() "source",
    scope       = scope
  )

}

test_that("a usable cache entry means no build is needed", {

  renv_tests_scope()
  renv_test_preflight_scope_cache()

  bread <- list(Package = "bread", Version = "1.0.0", Source = "Repository")
  expect_equal(renv_install_preflight_binaries_type(bread, getwd()), "none")

})

test_that("a requested rebuild skips the cache during preflight", {

  renv_tests_scope()
  renv_test_preflight_scope_cache()

  # the installer consults the restore state for the rebuild request
  renv_scope_restore(project = getwd(), rebuild = "bread")

  bread <- list(Package = "bread", Version = "1.0.0", Source = "Repository")
  expect_equal(renv_install_preflight_binaries_type(bread, getwd()), "source")

})

test_that("a disabled cache is not consulted during preflight", {

  renv_tests_scope()
  renv_test_preflight_scope_cache()
  renv_scope_options(renv.config.cache.enabled = FALSE)

  bread <- list(Package = "bread", Version = "1.0.0", Source = "Repository")
  expect_equal(renv_install_preflight_binaries_type(bread, getwd()), "source")

})

test_that("install() checks the resolved set of packages for source builds", {

  skip_on_cran()
  renv_tests_scope("breakfast")

  checked <- NULL
  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_install_preflight_binaries",
    replacement = function(records, project) {
      checked <<- names(records)
      TRUE
    }
  )

  # the dependencies of breakfast need installing too
  install("breakfast")
  expect_setequal(checked, c("breakfast", "bread", "oatmeal", "toast"))

})
