
test_that("invalid lockfile entries are reported", {

  renv_tests_scope()
  renv_scope_options(repos = NULL)

  expect_warning(renv_load_r(getwd(), NULL))
  expect_warning(renv_load_r(getwd(), list()))

  # this used to be a warning, but now we just write as an info message
  # expect_warning(renv_load_r(getwd(), list(Version = "1.0.0")))

})

test_that("renv/settings.R is sourced on load if available", {
  renv_tests_scope()
  ensure_directory("renv")
  writeLines("options(renv.test.dummy = 1)", con = "renv/settings.R")
  renv_load_settings(getwd())
  expect_equal(getOption("renv.test.dummy"), 1)
  options(renv.test.dummy = NULL)
})

test_that("errors when sourcing user profile are reported", {
  skip_on_cran()
  renv_tests_scope()
  renv_scope_options(renv.config.user.profile = TRUE)
  profile <- renv_scope_tempfile("renv-profile-", fileext = ".R")
  writeLines("stop(1)", con = profile)
  renv_scope_envvars(R_PROFILE_USER = profile)
  tryCatch(expect_warning(renv_load_rprofile(getwd())), error = identity)
})

test_that("load() installs packages if needed", {

  renv_tests_scope("breakfast")
  renv_scope_envvars(RENV_CONFIG_STARTUP_QUIET = "FALSE")

  install("bread")
  init()
  unlink("renv/library", recursive = TRUE)

  load()
  expect_true(renv_package_installed("breakfast"))

})

test_that("load() reports on problems", {

  renv_scope_libpaths()
  renv_tests_scope()
  renv_scope_envvars(RENV_CONFIG_STARTUP_QUIET = "FALSE")

  renv_tests_scope("egg")
  init()
  record("egg@2.0.0")

  expect_snapshot(load())

})

test_that("load() delegates to base::load() when appropriate", {

  renv_tests_scope()

  value <- 42
  save(value, file = ".RData")
  rm(value)

  load(".RData")
  expect_equal(value, 42)
  rm(value)

  load(file = ".RData")
  expect_equal(value, 42)
  rm(value)

  envir <- new.env(parent = emptyenv())
  load(".RData", envir = envir)
  expect_equal(envir$value, 42)

})

test_that("renv_load_check_namespaces flags packages loaded from outside libpaths", {

  # https://github.com/rstudio/renv/issues/2300 -- packages loaded
  # from a library that isn't part of the project's active .libPaths()
  # break renv's encapsulation; warn about them on project load.

  renv_tests_scope()
  renv_scope_options(renv.caution.verbose = TRUE)

  # install bread into a "user" library and load its namespace from there
  userlib <- renv_scope_tempfile("renv-userlib-")
  ensure_directory(userlib)
  install("bread", library = userlib)

  # indirect the package name through a variable so R CMD check doesn't
  # flag bread as an undeclared dependency
  pkg <- "bread"
  renv_scope_libpaths(c(userlib, .libPaths()))
  loadNamespace(pkg)
  defer(unloadNamespace(pkg))

  # restrict .libPaths() to just the project lib and .Library; bread is
  # still loaded but its location is no longer visible
  projlib <- renv_scope_tempfile("renv-projlib-")
  ensure_directory(projlib)
  renv_scope_libpaths(c(projlib, .Library))

  # the check only considers namespaces which were loaded before renv began
  # loading the project; load() normally records these for us
  renv_scope_binding(the, "load_namespaces", loadedNamespaces())

  # we test the helper directly; renv_load_check() only wires it up
  # during autoloader-driven startup, which doesn't fire in tests
  expect_output(
    result <- renv_load_check_namespaces(getwd()),
    "bread"
  )
  expect_false(result)

})

test_that("renv_load_check_namespaces is silent when all loaded packages are on libpaths", {

  renv_tests_scope()
  renv_scope_options(renv.caution.verbose = TRUE)

  # extend .libPaths() to include every location from which a non-base
  # namespace was loaded; the check should then find no externals
  loaded <- setdiff(loadedNamespaces(), c(renv_packages_base(), "renv"))
  paths <- vapply(loaded, function(p) dirname(renv_namespace_path(p)), character(1))
  renv_scope_libpaths(unique(c(paths, .libPaths())))
  renv_scope_binding(the, "load_namespaces", loadedNamespaces())

  expect_silent(result <- renv_load_check_namespaces(getwd()))
  expect_true(result)

})

test_that("renv_load_check_namespaces tolerates packages linked from the cache", {

  # https://github.com/rstudio/renv/issues/2344 -- R records a namespace's
  # path in resolved form, so a package linked into the library from the
  # renv cache must not be mistaken for one loaded from outside .libPaths()

  skip_on_os("windows")

  renv_tests_scope()
  renv_scope_options(renv.caution.verbose = TRUE)

  # install bread, then stage it as the cache does: the real package lives
  # outside the library, which merely holds a symlink pointing at it
  cache <- renv_scope_tempfile("renv-cache-")
  ensure_directory(cache)
  install("bread", library = cache)

  projlib <- renv_scope_tempfile("renv-projlib-")
  ensure_directory(projlib)

  pkg <- "bread"
  file.symlink(file.path(cache, pkg), file.path(projlib, pkg))

  renv_scope_libpaths(c(projlib, .Library))
  loadNamespace(pkg, lib.loc = projlib)
  defer(unloadNamespace(pkg))

  # consider only bread, so that the check's verdict is unambiguous
  renv_scope_binding(the, "load_namespaces", pkg)

  # the namespace path points into the cache, not the library ...
  expect_false(dirname(renv_namespace_path(pkg)) %in% renv_libpaths_all())

  # ... but the package is reachable via the library, so it's managed
  expect_silent(result <- renv_load_check_namespaces(getwd()))
  expect_true(result)

})

test_that("renv_load_check_namespaces ignores namespaces renv loaded itself", {

  # https://github.com/rstudio/renv/issues/2344 -- renv loads BiocManager
  # while resolving Bioconductor repositories, well after project load has
  # begun; such packages must not be reported as loaded before renv started

  renv_tests_scope()
  renv_scope_options(renv.caution.verbose = TRUE)

  # oatmeal lives in the project library; bread does not
  projlib <- renv_scope_tempfile("renv-projlib-")
  ensure_directory(projlib)
  install("oatmeal", library = projlib)

  userlib <- renv_scope_tempfile("renv-userlib-")
  ensure_directory(userlib)
  install("bread", library = userlib)

  managed <- "oatmeal"
  external <- "bread"

  renv_scope_libpaths(c(projlib, userlib, .Library))
  loadNamespace(managed)
  defer(unloadNamespace(managed))
  loadNamespace(external)
  defer(unloadNamespace(external))

  # drop bread's library, leaving it loaded from outside .libPaths()
  renv_scope_libpaths(c(projlib, .Library))

  # renv loaded bread only after project load began, so it isn't reported
  renv_scope_binding(the, "load_namespaces", managed)
  expect_silent(result <- renv_load_check_namespaces(getwd()))
  expect_true(result)

  # whereas had it been loaded beforehand, it would be
  renv_scope_binding(the, "load_namespaces", c(managed, external))
  expect_output(result <- renv_load_check_namespaces(getwd()), external)
  expect_false(result)

})
