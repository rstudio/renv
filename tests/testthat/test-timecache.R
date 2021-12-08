
context("Timecache")

test_that("the time cache can be used to cache values for some time", {

  skip_on_cran()

  # we should get the same value of subsequent runs
  old <- replicate(10, timecache("key", runif(1), limit = 0.4))
  expect_true(length(unique(old)) == 1L)

  # sleep for a second
  Sys.sleep(0.5)

  # try again; we should get new values
  new <- replicate(10, timecache("key", runif(1), limit = 0.4))
  expect_true(length(unique(new)) == 1L)

  # confirm they're different
  expect_false(identical(unique(old), unique(new)))

})
