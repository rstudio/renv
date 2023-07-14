
test_that("we can purge packages from the cache", {

  renv_tests_scope("breakfast")

  init()

  expect_true("breakfast" %in% basename(renv_cache_list()))
  purge("breakfast")
  expect_false("breakfast" %in% basename(renv_cache_list()))

})
