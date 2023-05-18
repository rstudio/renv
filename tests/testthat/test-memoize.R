
test_that("memoization works as expected", {

  global <- 0L
  scope <- basename(renv_scope_tempfile("renv-memoize-"))

  value <- memoize(
    scope = scope,
    key   = "example",
    value = global <- global + 1L
  )

  expect_equal(value, 1L)
  expect_equal(global, 1L)

  value <- memoize(
    scope = scope,
    key   = "example",
    value = global <- global + 1L
  )

  expect_equal(value, 1L)
  expect_equal(global, 1L)

  value <- memoize(
    scope = scope,
    key   = "example2",
    value = global <- global + 1L
  )

  expect_equal(value, 2L)
  expect_equal(global, 2L)

})

test_that("memoize avoids evaluating expression multiple times", {

  value <- 0L
  scope <- basename(renv_scope_tempfile("renv-memoize-"))

  memoize("example", { value <- value + 1L }, scope = scope)
  memoize("example", { value <- value + 1L }, scope = scope)

  expect_equal(value, 1L)

})

