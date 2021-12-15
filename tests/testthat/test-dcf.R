
context("DCF")

test_that("we can read different types of DCFs", {

  expected <- list(A = "1", B = "2")

  # plain jane
  actual <- renv_dcf_read(text = "A: 1\nB: 2")
  expect_equal(actual, expected)

  # extra whitespace between fields
  actual <- renv_dcf_read(text = "A: 1\n\nB: 2\n")
  expect_equal(actual, expected)

})

test_that("we allow for unindented continuations", {

  actual <- renv_dcf_read(text = "A: This field\nisn't indented.\nB: 42")
  expected <- list(A = "This field\nisn't indented.", B = "42")
  expect_equal(actual, expected)

})
