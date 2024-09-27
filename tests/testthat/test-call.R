
test_that("renv_call_matches() works as expected", {
  
  call <- quote(foo(1, 2))
  expect_true(renv_call_matches(call, "foo"))
  expect_true(renv_call_matches(call, "foo", nargs = 2L))
  expect_false(renv_call_matches(call, "bar"))
  expect_false(renv_call_matches(call, "bar", nargs = 1L))
  expect_true(renv_call_matches(call, names = c("foo", "bar")))
  expect_true(renv_call_matches(call, names = c("foo", "bar"), nargs = 2L))

})
