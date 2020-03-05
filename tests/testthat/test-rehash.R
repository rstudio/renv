
context("Rehash")

test_that("rehash() migrates cached packages as expected", {

  on.exit(Sys.unsetenv("RENV_CACHE_VERSION"), add = TRUE)

  tempcache <- tempfile("renv-cache-")
  on.exit(unlink(tempcache, recursive = TRUE), add = TRUE)
  renv_scope_envvars(RENV_PATHS_CACHE = tempcache)

  Sys.setenv(RENV_CACHE_VERSION = "v4")
  renv_tests_scope("breakfast")
  init()

  cached <- renv_cache_list()
  expect_length(cached, 4L)
  expect_match(cached, "/v4/", fixed = TRUE)

  Sys.setenv(RENV_CACHE_VERSION = "v5")
  rehash(prompt = FALSE)
  cached <- renv_cache_list()
  expect_length(cached, 4L)
  expect_match(cached, "/v5/", fixed = TRUE)

})
