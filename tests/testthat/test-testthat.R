
# the condition handler in our test_that wrapper should prevent
# this from being a real error; it should just be a skip
test_that("errors can be caught and skipped", {
  stop("download failed [error code 22]")
})

test_that("tests needing the GitHub API are skipped when its rate limit is low", {

  limits <- list(resources = list(core = list(remaining = 99L)))
  expect_condition(skip_if_github_rate_limited(limits), class = "skip")

  limits <- list(resources = list(core = list(remaining = 100L)))
  expect_no_condition(skip_if_github_rate_limited(limits), class = "skip")

})
