
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

  expect_silent(result <- renv_load_check_namespaces(getwd()))
  expect_true(result)

})
