
test_that("truthy() behaves as expected", {

  expect_true(truthy(TRUE))
  expect_true(truthy("TRUE"))
  expect_true(truthy("true"))
  expect_true(truthy(1))
  expect_false(truthy(0))

  expect_true(truthy(NULL, default = TRUE))
  expect_true(truthy(NA, default = TRUE))
  expect_true(truthy(numeric(), default = TRUE))

})
