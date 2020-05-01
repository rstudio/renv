
context("Paths")

test_that("all renv paths live within tempdir() during tests", {
  renv_tests_scope()
  info <- c(root = renv_paths_root(), tempdir = tempdir())
  expect_true(renv_path_within(renv_paths_root(), tempdir()), info = info)
})

test_that("the cache path can be set through an environment variable", {
  cachepath <- renv_tempfile("renv-cache-")
  renv_scope_envvars(RENV_PATHS_CACHE = cachepath)
  expect_true(startswith(renv_paths_cache(), cachepath))
})

test_that("we can construct paths to multiple files with path APIs", {
  root <- renv_paths_root()
  files <- renv_paths_root(c("A", "B", "C"), c("a", "b", "c"))
  expected <- file.path(root, c("A/a", "B/b", "C/c"))
  expect_equal(files, expected)
})

test_that("RENV_PATHS_PREFIX is respected", {

  os <- "ubuntu-bionic"
  renv_scope_envvars(RENV_PATHS_PREFIX = os)

  cachepath <- renv_paths_cache()
  expect_true(grepl(os, cachepath, fixed = TRUE))

  libpath <- renv_paths_library()
  expect_true(grepl(os, libpath, fixed = TRUE))

  prefix <- renv_prefix_platform()
  expect_true(startswith(prefix, os))

})
