
test_that("truthy() handles character inputs", {

  expect_true(truthy("TRUE"))
  expect_true(truthy("true"))
  expect_true(truthy("1"))
  expect_false(truthy("false"))
  expect_false(truthy("0"))

})

test_that("truthy() handles integers and logicals", {

  expect_true(truthy(TRUE))
  expect_false(truthy(FALSE))

  expect_true(truthy(1))
  expect_false(truthy(0))

})

test_that("truthy() returns default for invalid inputs", {

  expect_true(truthy(mean, default = TRUE))
  expect_true(truthy(NULL, default = TRUE))
  expect_true(truthy(NA, default = TRUE))
  expect_true(truthy(numeric(), default = TRUE))
  expect_true(truthy(1:3, default = TRUE))

})
