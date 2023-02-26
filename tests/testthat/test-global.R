
context("Global")

test_that("global() only evaluates value once", {

  name <- basename(tempfile("renv-example-"))
  on.exit(renv_global_clear(name), add = TRUE)

  value <- 0L
  global(name, value <<- value + 1L)
  global(name, value <<- value + 1L)

  expect_equal(global(name, value <<- value + 1L), 1L)

})
