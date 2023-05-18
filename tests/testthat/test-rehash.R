
test_that("rehash() migrates cached packages as expected", {

  tempcache <- renv_scope_tempfile("renv-cache-")
  renv_scope_envvars(RENV_PATHS_CACHE = tempcache)
  renv_scope_envvars(RENV_CACHE_VERSION = "v4")
  renv_tests_scope("breakfast")
  init()

  cached <- renv_cache_list()
  expect_length(cached, 4L)
  expect_match(cached, "/v4/", fixed = TRUE)

  renv_scope_envvars(RENV_CACHE_VERSION = "v5")
  rehash(prompt = FALSE)
  cached <- renv_cache_list()
  expect_length(cached, 4L)
  expect_match(cached, "/v5/", fixed = TRUE)

})
