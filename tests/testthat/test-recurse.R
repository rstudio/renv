
test_that("recurse() can handle missing objects", {
  data <- substitute(list(a = A), list(A = quote(expr = )))
  expect_no_error(recurse(data, function(node) print(node)))
})

test_that("recurse() can handle lists", {
  
  data <- list(
    list(a = 1, b = 2),
    list(
      list(c = 3, d = list(4))
    )
  )
  
  items <- list()
  recurse(data, function(el) {
    if (is.numeric(el))
      items[[length(items) + 1L]] <<- el
  })
  
  expect_equal(items, list(1, 2, 3, 4))
  
})
