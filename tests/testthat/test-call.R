test_that("renv_is_call() works as expected", {
  call <- quote(foo(1, 2))

  expect_true(renv_is_call(call))
  expect_true(renv_is_call(call, name = "foo"))
  expect_true(renv_is_call(call, name = c("foo", "bar")))
  expect_true(renv_is_call(call, n_args = 2))

  expect_false(renv_is_call(call, "bar"))
  expect_false(renv_is_call(call, n_args = 1))

  call <- quote(foo()(1, 2))
  expect_true(renv_is_call(call))
  expect_false(renv_is_call(call, "foo"))
})
