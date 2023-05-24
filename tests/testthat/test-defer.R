
test_that("defer evaluates in appropriate environment", {

  foo <- function() {
    writeLines("+ foo")
    defer(writeLines("> foo"),               environment())
    defer(writeLines("> foo.parent"),        parent.frame(1))
    defer(writeLines("> foo.parent.parent"), parent.frame(2))
    writeLines("- foo")
  }

  bar <- function() {
    writeLines("+ bar")
    foo()
    writeLines("- bar")
  }

  baz <- function() {
    writeLines("+ baz")
    bar()
    writeLines("- baz")
  }

  output <- capture.output(baz())
  expected <- c(
    "+ baz",
    "+ bar",
    "+ foo",
    "- foo",
    "> foo",
    "- bar",
    "> foo.parent",
    "- baz",
    "> foo.parent.parent"
  )

  expect_identical(output, expected)

})

test_that("defer runs handles in LIFO order", {
  x <- double()
  local({
    defer(x <<- c(x, 1))
    defer(x <<- c(x, 2))
    defer(x <<- c(x, 3))
  })

  expect_equal(x, c(3, 2, 1))
})

test_that("defer captures arguments properly", {

  foo <- function(x) {
    defer(writeLines(x), envir = parent.frame())
  }

  bar <- function(y) {
    writeLines("+ bar")
    foo(y)
    writeLines("- bar")
  }

  output <- capture.output(bar("> foo"))
  expected <- c("+ bar", "- bar", "> foo")
  expect_identical(output, expected)

})

test_that("defer works with arbitrary expressions", {

  foo <- function(x) {
    defer({
      x + 1
      writeLines("> foo")
    }, envir = parent.frame())
  }

  bar <- function() {
    writeLines("+ bar")
    foo(1)
    writeLines("- bar")
  }

  output <- capture.output(bar())
  expected <- c("+ bar", "- bar", "> foo")
  expect_identical(output, expected)

})


test_that("renv_defer_execute can run handlers earlier", {

  # test with current environment
  x <- 1
  defer(rm(list = "x"))
  renv_defer_execute(environment())
  expect_equal(exists("x", inherits = FALSE), FALSE)

  # test with global environment
  env <- globalenv()
  env$x <- 1
  defer(rm(list = "x", envir = env), envir = env)
  renv_defer_execute(env)
  expect_equal(exists("x", inherits = FALSE), FALSE)

})
