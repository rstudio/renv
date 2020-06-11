
context("Restore")

test_that("library permissions are validated before restore", {
  skip_on_os("windows")
  inaccessible <- renv_tempfile()
  dir.create(inaccessible, mode = "0100")
  renv_scope_options(renv.verbose = FALSE)
  expect_false(renv_install_preflight_permissions(inaccessible))
})

test_that("we can restore packages after init", {
  skip_on_cran()
  renv_tests_scope("breakfast")

  renv::init()

  libpath <- renv_paths_library()
  before <- list.files(libpath)

  unlink(renv_paths_library(), recursive = TRUE)
  renv::restore()

  after <- list.files(libpath)
  expect_setequal(before, after)

})

test_that("restore can recover when required packages are missing", {
  skip_on_cran()
  renv_tests_scope("breakfast")
  renv::init()

  local({
    renv_scope_sink()
    renv::remove("oatmeal")
    renv::snapshot(force = TRUE)
    unlink(renv_paths_library(), recursive = TRUE)
    renv::restore()
  })

  expect_true(renv_package_installed("oatmeal"))

})

test_that("restore(clean = TRUE) removes packages not in the lockfile", {

  renv_tests_scope("oatmeal")
  renv::init()

  renv_scope_options(renv.config.auto.snapshot = FALSE)
  renv::install("bread")
  expect_true(renv_package_installed("bread"))

  renv::restore(clean = TRUE)
  expect_false(renv_package_installed("bread"))

})

test_that("renv.records can be used to override records during restore", {

  renv_tests_scope("bread")
  renv::init()

  renv::install("bread@0.1.0")
  renv::snapshot()
  expect_equal(renv_package_version("bread"), "0.1.0")

  bread <- list(Package = "bread", Version = "1.0.0", Source = "CRAN")
  overrides <- list(bread = bread)
  renv_scope_options(renv.records = overrides)

  renv::restore()
  expect_equal(renv_package_version("bread"), "1.0.0")

})

test_that("install.staged works as expected", {

  renv_tests_scope("breakfast")

  renv::init()
  renv::remove("breakfast")
  renv::purge("breakfast")

  install.opts <- list(breakfast = "--version")

  local({
    renv_scope_options(renv.config.install.staged = TRUE)
    renv_scope_options(install.opts = install.opts)
    templib <- tempfile("renv-templib-")
    expect_error(renv::restore(library = templib))
    files <- list.files(templib)
    expect_true(length(files) == 0L)
  })

  local({
    renv_scope_options(renv.config.install.staged = FALSE)
    renv_scope_options(install.opts = install.opts)
    templib <- tempfile("renv-templib-")
    expect_error(renv::restore(library = templib))
    files <- list.files(templib)
    expect_true(length(files) != 0L)
  })

  install("breakfast")

})

test_that("renv::restore(lockfile = '/path/to/lockfile') works", {

  renv_tests_scope("bread")

  renv::init()

  unlink(paths$library(), recursive = TRUE)
  renv::restore(lockfile = "renv.lock")
  expect_true(renv_package_installed("bread"))

  unlink(paths$library(), recursive = TRUE)
  lockfile <- renv_lockfile_load(project = getwd())
  renv::restore(lockfile = "renv.lock")
  expect_true(renv_package_installed("bread"))

})

test_that("renv::restore(packages = <...>) works", {
  renv_tests_scope("breakfast")
  renv::init()
  unlink(paths$library(), recursive = TRUE)
  renv::restore(packages = "toast")
  expect_length(list.files(paths$library()), 2L)
  expect_true(renv_package_installed("bread"))
  expect_true(renv_package_installed("toast"))
})

test_that("restore ignores packages of incompatible architecture", {

  renv_tests_scope(c("unixonly", "windowsonly"))
  renv::init()

  if (renv_platform_unix()) {

    expect_true(renv_package_installed("unixonly"))
    expect_false(renv_package_installed("windowsonly"))

    lockfile <- renv_lockfile_read("renv.lock")
    package <- lockfile$Packages$unixonly
    expect_identical(package$OS_type, "unix")

    remove("unixonly")
    restore()
    expect_true(renv_package_installed("unixonly"))

  } else {

    expect_true(renv_package_installed("windowsonly"))
    expect_false(renv_package_installed("unixonly"))

    lockfile <- renv_lockfile_read("renv.lock")
    package <- lockfile$Packages$windowsonly
    expect_identical(package$OS_type, "windows")

    remove("windowsonly")
    restore()
    expect_true(renv_package_installed("windowsonly"))

  }

})
