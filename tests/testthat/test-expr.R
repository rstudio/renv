
test_that("expr() works as expected", {

  value <- 42
  actual <- expr({
    x <- !!value
  })

  expected <- quote({
    x <- 42
  })

  expect_identical(actual, expected)

})

test_that("expr() also supports expressions", {

  actual <- expr({
    pid <- !!Sys.getpid()
  })

  expected <- substitute({
    pid <- id
  }, list(id = Sys.getpid()))

  expect_identical(actual, expected)

})

test_that("expr() repairs parse trees", {

  a <- TRUE; b <- FALSE
  actual <- expr(!!a + !!b)
  expected <- quote(TRUE + FALSE)
  expect_identical(actual, expected)

})
