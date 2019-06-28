
context("Clean")

test_that("clean removes stale lockfiles", {

  renv_tests_scope("bread")

  renv::init()
  library <- renv_paths_library()

  # old temporary directory
  tmpdir <- tempfile(tmpdir = library)
  ensure_directory(tmpdir)

  # stale lockfile
  lockpath <- file.path(library, "00LOCK-package")
  ensure_directory(lockpath)
  Sys.setFileTime(lockpath, Sys.time() - 36000)

  # used package
  suppressWarnings(renv::install("toast"))

  # clean up the project
  renv::clean()

  # check the project has been cleaned
  expect_false(file.exists(tmpdir))
  expect_false(file.exists(lockpath))
  expect_false(file.exists(file.path(library, "toast")))

  # check that toast is not in the cache
  cache <- renv_cache_list()
  expect_false("toast" %in% basename(cache))

})
