
test_that("once() returns TRUE only once", {

  method <- function() {
    expect_true(once())
    expect_false(once())
    expect_false(once())
  }

  method()
  expect_error(method())

})
