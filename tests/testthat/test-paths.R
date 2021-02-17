
context("Paths")

test_that("all renv paths live within tempdir() during tests", {
  renv_tests_scope()
  info <- c(root = renv_paths_root(), tempdir = tempdir())
  expect_true(renv_path_within(renv_paths_root(), tempdir()), info = info)
})

test_that("the cache path can be set through an environment variable", {
  cachepath <- renv_tempfile_path("renv-cache-")
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

  prefix <- renv_platform_prefix()
  expect_true(startswith(prefix, os))

})

test_that("RENV_PATHS_PREFIX is not normalized", {
  renv_scope_envvars(RENV_PATHS_PREFIX = ".")
  renv_paths_init()
  expect_identical(Sys.getenv("RENV_PATHS_PREFIX"), ".")
})

test_that("renv_path_normalize is correctly initialized", {

  expect_identical(
    renv_path_normalize,
    if (renv_platform_windows())
      renv_path_normalize_win32
    else
      renv_path_normalize_unix
  )

})

test_that("UTF-8 paths can be normalized", {

  skip_if(getRversion() < "4.0.0")

  name <- enc2utf8("\u4f60\u597d")  # nihao
  root <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
  path <- paste(root, name, sep = "/")
  expect_true(file.create(path))

  expected <- path
  actual   <- renv_path_normalize(path)
  expect_equal(expected, actual)

})
