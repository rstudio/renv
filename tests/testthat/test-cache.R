
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

  libpath <- renv_tempfile_path("renv-library")
  ensure_directory(libpath)
  renv_scope_envvars(RENV_PATHS_LIBRARY = file.path(libpath, "."))

  renv_tests_scope("bread")
  init()
  remove("bread")
  restore()

  bread <- system.file(package = "bread")
  expect_true(renv_file_type(bread) == "symlink")

})

test_that("malformed folders in the cache are ignored", {
  skip_on_cran()
  renv_tests_scope()

  cachepath <- tempfile("renv-cache-")
  renv_scope_envvars(RENV_PATHS_CACHE = cachepath)
  on.exit(unlink(cachepath, recursive = TRUE), add = TRUE)

  badpath <- renv_paths_cache("a-b/c-d/e-f/g-h/i-j")
  dir.create(dirname(badpath), recursive = TRUE)
  file.create(badpath)

  paths <- list.files(renv_paths_cache(), recursive = TRUE)
  expect_length(paths, 1)

  paths <- renv_cache_list()
  expect_length(paths, 0)

})

test_that("corrupt Meta/package.rds is detected", {
  skip_on_cran()
  renv_tests_scope()

  cachepath <- tempfile("renv-cache-")
  renv_scope_envvars(RENV_PATHS_CACHE = cachepath)
  on.exit(unlink(cachepath, recursive = TRUE), add = TRUE)

  init()
  install("bread")

  path <- renv_cache_find(list(Package = "bread", Version = "1.0.0"))
  expect_true(nzchar(path) && file.exists(path))

  metapath <- file.path(path, "Meta/package.rds")
  expect_true(file.exists(metapath))

  writeLines("whoops!", con = file.path(path, "Meta/package.rds"))

  diagnostics <- renv_cache_diagnose(verbose = FALSE)

  expect_true(is.data.frame(diagnostics))
  expect_true(nrow(diagnostics) == 1)
  expect_true(diagnostics$Package == "bread")
  expect_true(diagnostics$Version == "1.0.0")

})

test_that("multiple cache directories are used", {
  skip_on_cran()
  skip_on_os("windows") # setting folder permissions is a bit more complex on windows

  chmod <- function(path, mode = c("read", "read+write")) {
    mode <- match.arg(mode)
    path <- normalizePath(path)
    stopifnot(file.exists(path))
    # '5' (read and execute) is the minimum permission that will work.
    # With '4' (read only) things like dir() don't work anymore.
    numbers <- if (mode == "read") "555" else "777"
    system(sprintf("chmod %s %s", numbers, path))
  }

  # use multiple temporary caches for this test
  tempcache1 <- tempfile("renv-tempcache-")
  ensure_directory(tempcache1)
  tempcache2 <- tempfile("renv-tempcache-")
  ensure_directory(tempcache2)

  on.exit({
    unlink(tempcache1, recursive = TRUE)
    unlink(tempcache2, recursive = TRUE)
  }, add = TRUE)

  # add both packages to the cache
  renv_scope_envvars(RENV_PATHS_CACHE = paste(tempcache1, tempcache2, sep = ";"))

  # initialize project
  renv_tests_scope()
  renv::init()

  # there should be two paths in the cache
  expect_length(renv::paths$cache(), 2L)

  # install bread to first cache path
  renv::install("bread")

  # test that there is one package (bread) and it is installed in the first cache
  cache <- renv_cache_list()
  expect_length(cache, 1L)
  expect_true(startsWith(cache[basename(cache) == "bread"], tempcache1))

  # make the first cache read only
  chmod(renv::paths$cache()[1L], "read")

  # install oatmeal to second cache path
  renv::install("oatmeal")

  # test that there are 2 packages and the latest package (oatmeal) is installed in the second cache
  cache <- renv_cache_list()
  expect_length(cache, 2L)
  expect_true(startsWith(cache[basename(cache) == "oatmeal"], tempcache2))

  # make the first cache read+write again, should now install into the first cache again
  chmod(renv::paths$cache()[1L], "read+write")

  renv::install("toast")

  # test that there are 3 packages and the latest package (toast) is installed in the first cache
  cache <- renv_cache_list()
  expect_length(cache, 3L)
  expect_true(startsWith(cache[basename(cache) == "toast"], tempcache1))

})
