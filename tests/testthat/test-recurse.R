
test_that("recurse() can handle missing objects", {
  data <- substitute(list(a = A), list(A = quote(expr = )))
  expect_no_error(recurse(data, function(node) print(node)))
})
