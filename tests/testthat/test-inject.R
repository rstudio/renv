
test_that("inject() works as expected", {

  value <- 42
  actual <- inject({ x <- .(value) })
  expected <- quote({ x <- 42 })
  expect_identical(actual, expected)

})

test_that("inject() also supports expressions", {

  actual <- inject({ pid <- .(Sys.getpid()) })
  expected <- substitute({ pid <- id }, list(id = Sys.getpid()))
  expect_identical(actual, expected)

})

test_that("inject() works recursively", {

  a <- as.symbol("b"); b <- 42
  actual <- inject(.(.(a)))
  expected <- 42
  expect_identical(actual, expected)

})
