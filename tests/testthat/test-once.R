
context("Once")

test_that("renv_once() returns TRUE only once", {

  method <- function() {
    expect_true(renv_once())
    expect_false(renv_once())
    expect_false(renv_once())
  }

  method()
  expect_error(method())

})
