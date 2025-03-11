
test_that("hydrate does not change library paths", {

  renv_tests_scope()

  lib <- renv_scope_tempfile()
  ensure_directory(lib)
  .libPaths(lib)

  before <- .libPaths()
  hydrate()
  after <- .libPaths()

  expect_identical(before, after)

})

test_that("hydrate(update = FALSE) does not update older packages", {

  renv_tests_scope("bread")
  init()

  # set up project with older version of bread
  install("bread@0.1.0")

  # add dependency on toast
  writeLines("library(toast)", con = "deps2.R")

  # set up library for hydration
  sourcelib <- renv_scope_tempfile("renv-source-")
  ensure_directory(sourcelib)
  install("toast", library = sourcelib)

  # try hydrating without update
  expect_false(renv_package_installed("toast"))
  hydrate(sources = sourcelib, update = FALSE)
  expect_true(renv_package_installed("toast"))
  expect_true(renv_package_version("toast") == "1.0.0")
  expect_true(renv_package_version("bread") == "0.1.0")

  # try hydrating with update
  hydrate(sources = sourcelib, update = TRUE)
  expect_true(renv_package_version("bread") == "1.0.0")

})

test_that("hydrate succeeds when package installed into user library", {

  # use alternate (empty) cache for this test
  cachedir <- renv_scope_tempfile("renv-cache-")
  ensure_directory(cachedir)
  renv_scope_envvars(RENV_PATHS_CACHE = cachedir)

  # initialize empty project
  project <- renv_tests_scope()
  init()

  # make sure 'bread' isn't in the cache currently
  # install 'bread' into a user library path
  userlib <- renv_scope_tempdir("renv-library-")
  install("bread", library = userlib)

  # try to hydrate from that source
  hydrate(packages = "bread", sources = userlib)
  expect_true(renv_package_installed("bread"))

})
