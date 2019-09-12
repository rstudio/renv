
context("Errors")

test_that("tracebacks are captured by catch", {
  skip_if(getRversion() < "3.3.0")

  output <- catch(stop("ouch"))
  traceback <- output$traceback

  expect_true(is.character(traceback))
  expect_true(length(traceback) > 0)
})
