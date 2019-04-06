
context("Paths")

test_that("all renv paths live within tempdir() during tests", {
  info <- c(root = renv_paths_root(), tempdir = tempdir())
  expect_true(path_within(renv_paths_root(), tempdir()), info = info)
})

test_that("the cache path can be set through an environment variable", {
  cachepath <- tempfile()
  renv_scope_envvars(RENV_PATHS_CACHE = cachepath)
  expect_true(startswith(renv_paths_cache(), cachepath))
})
