
test_that("recurse() can handle missing objects", {
  data <- substitute(list(a = A), list(A = quote(expr = )))
  expect_no_error(recurse(data, function(node) force(node)))
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
  
  items <- list()
  recurse(data, function(el, ignored) {
    if (is.numeric(el))
      items[[length(items) + 1L]] <<- el
  }, ignored = 42)
  
  expect_equal(items, list(1, 2, 3, 4))
  
})

test_that("recurse() can handle dots", {
  
  counter <- 0L
  recurse(list(1, list(2, list(3, list(4, list(5))))), function(node) {
    if (is.list(node))
      counter <<- counter + 1L
  })
  expect_equal(counter, 5L)
  
  counter <- 0L
  recurse(list(1, list(2, list(3, list(4, list(5))))), function(node, extra) {
    expect_equal(extra, 42)
    if (is.list(node))
      counter <<- counter + 1L
  }, extra = 42)
  expect_equal(counter, 5L)
  
})
