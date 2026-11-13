
test_that("tracebacks are captured by catch", {
  skip_if(getRversion() < "3.3.0")

  output <- catch(stop("ouch"))
  expect_true(length(output$calls) > 0)

  traceback <- renv_error_traceback(output)
  expect_true(is.character(traceback))
  expect_true(length(traceback) > 0)
})

test_that("catch() doesn't retain the calls of the last caught error", {
  output <- catch(stop("ouch"))
  expect_null(the$error_calls)
})
