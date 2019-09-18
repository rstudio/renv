
context("Activate")

test_that("renv can bootstrap itself if not installed", {
  skip_on_cran()

  renv_tests_scope()
  renv::init(bare = TRUE)
  args <- c("--slave", "-e", shQuote("library(renv)"))
  status <- system2(R(), args, stdout = FALSE, stderr = FALSE)
  expect_equal(status, 0L)

})
