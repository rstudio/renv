
test_that("library permissions are validated before restore", {
  skip_on_os("windows")
  inaccessible <- renv_scope_tempfile()
  dir.create(inaccessible, mode = "0100")
  expect_false(renv_install_preflight_permissions(inaccessible))
})

test_that("restore() gives an error if no lockfile exists", {
  renv_tests_scope()
  expect_false(file.exists("renv.lock"))
  expect_error(restore())
})

test_that("we can restore packages after init", {
  skip_on_cran()
  renv_tests_scope("breakfast")

  init()

  libpath <- renv_paths_library()
  before <- list.files(libpath)

  unlink(renv_paths_library(), recursive = TRUE)
  restore()

  after <- list.files(libpath)
  expect_setequal(before, after)

})

test_that("restore can recover when required packages are missing", {
  skip_on_cran()
  renv_tests_scope("breakfast")
  init()

  remove("oatmeal")
  snapshot(force = TRUE)
  unlink(renv_paths_library(), recursive = TRUE)
  restore()

  expect_true(renv_package_installed("oatmeal"))

})

test_that("restore(clean = TRUE) removes packages not in the lockfile", {

  renv_tests_scope("oatmeal")
  init()

  renv_scope_options(renv.config.auto.snapshot = FALSE)
  install("bread")
  expect_true(renv_package_installed("bread"))

  restore(clean = TRUE)
  expect_false(renv_package_installed("bread"))

})

test_that("renv.records can be used to override records during restore", {

  renv_tests_scope("bread")
  init()

  install("bread@0.1.0")
  snapshot()
  expect_equal(renv_package_version("bread"), "0.1.0")

  bread <- list(Package = "bread", Version = "1.0.0", Source = "CRAN")
  overrides <- list(bread = bread)
  renv_scope_options(renv.records = overrides)

  restore()
  expect_equal(renv_package_version("bread"), "1.0.0")

})

test_that("install.staged works as expected", {

  renv_tests_scope("breakfast")

  init()
  library <- renv_paths_library(project = getwd())

  install.opts <- list(breakfast = "--version")

  local({

    renv_scope_options(
      renv.config.install.staged = TRUE,
      renv.config.install.transactional = TRUE,
      install.opts = install.opts
    )

    renv_scope_envvars(RENV_PATHS_CACHE = renv_scope_tempfile())

    unlink(renv_paths_library(), recursive = TRUE)
    expect_error(restore())
    files <- list.files(library)
    expect_true(length(files) == 0L)

  })

  local({

    renv_scope_options(
      renv.config.install.staged = FALSE,
      renv.config.install.transactional = FALSE,
      install.opts = install.opts
    )

    renv_scope_envvars(RENV_PATHS_CACHE = renv_scope_tempfile())

    unlink(renv_paths_library(), recursive = TRUE)
    expect_error(restore())
    files <- list.files(library)
    expect_true(length(files) != 0L)

  })

})

test_that("restore(lockfile = '/path/to/lockfile') works", {

  renv_tests_scope("bread")

  init()

  unlink(paths$library(), recursive = TRUE)
  restore(lockfile = "renv.lock")
  expect_true(renv_package_installed("bread"))

  unlink(paths$library(), recursive = TRUE)
  lockfile <- renv_lockfile_load(project = getwd())
  restore(lockfile = "renv.lock")
  expect_true(renv_package_installed("bread"))

})

test_that("restore(packages = <...>) works", {
  renv_tests_scope("breakfast")
  init()
  unlink(paths$library(), recursive = TRUE)
  restore(packages = "toast")
  expect_length(list.files(paths$library()), 2L)
  expect_true(renv_package_installed("bread"))
  expect_true(renv_package_installed("toast"))
})

test_that("restore ignores packages of incompatible architecture", {

  renv_tests_scope(c("unixonly", "windowsonly"))
  init()

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

test_that("restore handled records without version set", {

  renv_tests_scope()

  # create dummy lockfile
  snapshot()

  # read lockfile and add record without version
  lockfile <- renv_lockfile_load(project = getwd())
  lockfile$Packages$bread <- list(Package = "bread", Source = "Repository")
  renv_lockfile_save(lockfile, project = getwd())

  # try to restore
  restore()

  # check for success
  expect_true(renv_package_installed("bread"))
  expect_equal(renv_package_version("bread"), "1.0.0")

})

test_that("restore doesn't re-use active library paths", {

  renv_tests_scope()
  renv_scope_options(renv.settings.snapshot.type = "all")

  lib1 <- renv_scope_tempfile("lib1")
  lib2 <- renv_scope_tempfile("lib2")
  ensure_directory(c(lib1, lib2))
  .libPaths(c(lib2, .libPaths()))

  install("bread", library = lib2)
  expect_true(renv_package_installed("bread", lib.loc = lib2))

  lockfile <- snapshot(library = lib2, lockfile = NULL)
  restore(library = lib1, lockfile = lockfile)
  expect_true(renv_package_installed("bread", lib.loc = lib1))

})

test_that("restore(exclude = <...>) excludes as expected", {

  renv_tests_scope("breakfast")
  init()

  remove(c("bread", "breakfast", "oatmeal", "toast"))
  restore(exclude = "breakfast")
  expect_false(renv_package_installed("breakfast"))

})

test_that("restore works with explicit Source", {

  renv_tests_scope("breakfast")
  init()

  renv_scope_envvars(
    RENV_PATHS_LOCAL = "",
    RENV_PATHS_CACHE = ""
  )

  record <- list(
    Package = "skeleton",
    Version = "1.0.0",
    Source  = renv_tests_path("local/skeleton/skeleton_1.0.0.tar.gz")
  )

  renv_test_retrieve(record)

  lockfile <- renv_lockfile_init(project = getwd())
  lockfile$Packages <- list(skeleton = record)
  renv_lockfile_write(lockfile, file = "renv.lock")
  remove("skeleton")

  restore()

  expect_true(renv_package_installed("skeleton"))
  expect_true(renv_package_version("skeleton") == "1.0.0")

})

test_that("restore() restores packages with broken symlinks", {

  skip_on_cran()
  renv_scope_options(renv.settings.cache.enabled = TRUE)
  renv_tests_scope("breakfast")
  init()

  # check it's installed
  pkgpath <- renv_package_find("breakfast")
  expect_true(renv_file_exists(pkgpath))

  # break the cache
  record <- list(Package = "breakfast", Version = "1.0.0")
  cachepath <- renv_cache_find(record)
  unlink(cachepath, recursive = TRUE)

  # check that it's broken
  expect_true(renv_file_broken(pkgpath))

  # try to restore
  restore()

  # check that we're happy again
  expect_false(renv_file_broken(pkgpath))
  expect_true(renv_file_exists(pkgpath))
  expect_true(renv_package_installed("breakfast"))

})

test_that("restore() also installs packages with broken symlinks", {
  skip_on_cran()
  skip_on_os("windows")
  renv_scope_options(renv.settings.cache.enabled = TRUE)
  project <- renv_tests_scope("breakfast")
  init()

  pkgpaths <- list.files(
    path = renv_paths_library(project = project),
    full.names = TRUE
  )

  links <- Sys.readlink(pkgpaths)
  expect_true(all(nzchar(links)))

  unlink(links, recursive = TRUE)
  expect_false(renv_package_installed("breakfast"))

  restore()
  expect_true(renv_package_installed("breakfast"))
})
