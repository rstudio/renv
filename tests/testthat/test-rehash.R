
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

test_that("rehash() moves mis-hashed packages and points users at repair()", {

  tempcache <- renv_scope_tempfile("renv-cache-")
  renv_scope_envvars(RENV_PATHS_CACHE = tempcache)
  renv_scope_envvars(RENV_CACHE_VERSION = "v5")
  renv_tests_scope("breakfast")
  init()

  # relocate one cached package to a bogus hash, mimicking a package that was
  # cached under a previous (stale) hashing scheme within the active cache
  entry <- renv_cache_list()[[1L]]
  hashdir <- dirname(entry)
  bogus <- file.path(dirname(hashdir), "0000000000000000")
  renv_file_move(hashdir, bogus)

  # rehash should move it back to its correct location and mention repair()
  renv_scope_options(renv.verbose = TRUE)
  output <- capture.output(rehash(prompt = FALSE))

  expect_true(file.exists(entry))
  expect_false(file.exists(bogus))
  expect_true(any(grepl("renv::repair()", output, fixed = TRUE)))

})
