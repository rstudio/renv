
context("Cache")

test_that("issues within the cache are reported", {
  skip_on_cran()

  # use a temporary cache for this test as we're going
  # to mutate and invalidate it
  tempcache <- tempfile("renv-tempcache-")
  ensure_directory(tempcache)
  on.exit(unlink(tempcache, recursive = TRUE), add = TRUE)
  renv_scope_envvars(RENV_PATHS_CACHE = tempcache)

  # initialize project
  renv_tests_scope("breakfast")
  renv::init()

  # find packages in the cache
  cache <- renv_cache_list()

  # diagnostics for missing DESCRIPTION
  bread <- renv_cache_list(packages = "bread")
  descpath <- file.path(bread, "DESCRIPTION")
  unlink(descpath)

  # diagnostics for bad hash
  breakfast <- renv_cache_list(packages = "breakfast")
  descpath <- file.path(breakfast, "DESCRIPTION")
  desc <- renv_description_read(descpath)
  desc$Version <- "2.0.0"
  renv_dcf_write(desc, file = descpath)

  # check problems explicitly
  problems <- renv_cache_diagnose(verbose = FALSE)
  expect_true(nrow(problems) == 2)

})

test_that("use.cache project setting is honored", {
  skip_on_os("windows")

  renv_tests_scope("breakfast")

  renv::init()

  packages <- list.files(renv_paths_library(), full.names = TRUE)
  types <- renv_file_type(packages)
  expect_true(all(types == "symlink"))

  renv::settings$use.cache(FALSE)

  packages <- list.files(renv_paths_library(), full.names = TRUE)
  types <- renv_file_type(packages)
  expect_true(all(types == "directory"))

  renv::settings$use.cache(TRUE)

  packages <- list.files(renv_paths_library(), full.names = TRUE)
  types <- renv_file_type(packages)
  expect_true(all(types == "symlink"))

})

test_that("package installation does not fail with non-writable cache", {
  skip_on_os("windows")

  renv_tests_scope()

  cache <- tempfile("renv-cache-")
  dir.create(cache, mode = "0555")
  renv_scope_envvars(RENV_PATHS_CACHE = cache)

  renv::init()
  records <- renv::install("bread")
  expect_true(records$bread$Package == "bread")

  location <- find.package("bread")
  type <- renv_file_type(location)
  expect_false(type == "symlink")

})

test_that("the cache is used even if RENV_PATHS_LIBRARY is non-canonical", {
  skip_on_os("windows")

  libpath <- renv_tempfile("renv-library")
  ensure_directory(libpath)
  renv_scope_envvars(RENV_PATHS_LIBRARY = file.path(libpath, "."))

  renv_tests_scope("bread")
  init()
  remove("bread")
  restore()

  bread <- system.file(package = "bread")
  expect_true(renv_file_type(bread) == "symlink")

})
