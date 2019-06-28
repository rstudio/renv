
context("Errors")

test_that("errors are reported through scoped handler", {

  output <- capture.output(
    tryCatch(stop("ouch"), error = renv_error_handler),
    type = "message"
  )

  expect_true(length(output) > 0)

})
