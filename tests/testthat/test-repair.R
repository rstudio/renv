
test_that("we can repair a broken project library", {

  skip_on_cran()
  renv_tests_scope("breakfast")

  init()

  # find breakfast in the cache, and delete it
  record <- list(Package = "breakfast", Version = "1.0.0")
  cachepath <- renv_cache_find(record)
  unlink(cachepath, recursive = TRUE)

  # check that the package no longer exists
  expect_false(renv_package_installed("breakfast"))

  # try to repair
  repair()

  # validate that we reinstalled it
  expect_true(renv_package_installed("breakfast"))

})

test_that("repair() uses the package version recorded in the lockfile", {

  skip_on_cran()
  renv_tests_scope("breakfast")

  init()

  # install older breakfast from archive
  install("breakfast@0.1.0")
  snapshot()

  # find breakfast in the cache, and delete it
  record <- list(Package = "breakfast", Version = "0.1.0")
  cachepath <- renv_cache_find(record)
  unlink(cachepath, recursive = TRUE)

  # check that the package no longer exists
  expect_false(renv_package_installed("breakfast"))

  # try to repair
  repair()

  # validate that we reinstalled it
  expect_true(renv_package_installed("breakfast"))
  expect_true(renv_package_version("breakfast") == "0.1.0")

})

test_that("repair() can update DESCRIPTION files for GitHub packages", {

  skip_on_cran()
  renv_tests_scope("skeleton")
  renv_scope_local()
  renv_scope_options(renv.config.cache.symlinks = FALSE)
  init()

  # mutate the installed package DESCRIPTION file, so that it appears
  # to be from GitHub
  descpath <- system.file("DESCRIPTION", package = "skeleton")
  desc <- renv_description_read(descpath)
  desc$Repository <- NULL
  desc$BugReports <- "https://github.com/kevinushey/skeleton/issues"
  renv_dcf_write(desc, file = descpath)

  metapath <- system.file("Meta/package.rds", package = "skeleton")
  meta <- readRDS(metapath)
  meta$DESCRIPTION <- desc
  saveRDS(meta, file = metapath)

  # try to repair, but cancel the request
  renv_scope_options(renv.menu.choice = 2L)
  expect_snapshot(. <- repair(), error = TRUE)

})
