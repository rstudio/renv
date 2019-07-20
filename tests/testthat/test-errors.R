
context("Errors")

test_that("errors are reported through scoped handler", {
  skip_if(getRversion() < "3.3.0")

  output <- capture.output(
    tryCatch(stop("ouch"), error = renv_error_handler),
    type = "message"
  )

  expect_true(length(output) > 0)
})
