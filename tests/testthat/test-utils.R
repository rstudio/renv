
context("Utils")

test_that("common utils work as expected", {
  expect_equal(NULL %NULL% 42, 42)
  expect_equal(lines(1, 2, 3), "1\n2\n3")

  if (nzchar(Sys.which("git")))
    expect_equal(git(), Sys.which("git"))
  else
    expect_error(git())
})

test_that("versions are compared as expected", {

  expect_equal(renv_version_compare("0.1.0", "0.2.0"), -1L)
  expect_equal(renv_version_compare("0.2.0", "0.2.0"), +0L)
  expect_equal(renv_version_compare("0.3.0", "0.2.0"), +1L)

})

test_that("inject inserts text at expected anchor point", {

  text <- c("alpha", "beta", "gamma")

  injected <- inject(text, "beta", "BETA")
  expect_equal(injected, c("alpha", "BETA", "gamma"))

  injected <- inject(text, "BETA", "BETA", "beta")
  expect_equal(injected, c("alpha", "beta", "BETA", "gamma"))

})

test_that("renv_path_aliased() correctly forms aliased path", {
  path <- "~/some/path"
  expanded <- path.expand(path)
  expect_equal(path, renv_path_aliased(expanded))
})

test_that("sink captures both stdout and stderr", {

  file <- tempfile("renv-sink-", fileext = ".log")

  osinks <- sink.number(type = "output")
  msinks <- sink.number(type = "message")

  local({
    renv_scope_sink(file)
    writeLines("stdout", con = stdout())
    writeLines("stderr", con = stderr())
  })

  contents <- readLines(file)
  expect_equal(contents, c("stdout", "stderr"))

  expect_equal(sink.number(type = "output"),  osinks)
  expect_equal(sink.number(type = "message"), msinks)


})

test_that("find() returns first non-null matching value", {

  data <- list(x = 1, y = 2, z = 3)

  value <- find(data, function(datum) {
    if (datum == 2)
      return(42)
  })
  expect_equal(value, 42)

  value <- find(data, function(datum) {
    if (datum == 4)
      return(42)
  })
  expect_null(value)

})

test_that("recursing() reports if we're recursing", {

  f <- function(i) {

    if (recursing())
      expect_true(i == 2)
    else
      expect_true(i == 1)


    if (i < 2)
      f(i + 1)

    if (recursing())
      expect_true(i == 2)
    else
      expect_true(i == 1)

  }

  f(1)


})

test_that("sys.call(sys.parent()) does what we think it does", {

  inner <- function() { as.character(sys.call(sys.parent())[[1L]]) }
  outer <- function() { inner() }
  expect_equal(outer(), "outer")

})

test_that("new() creates objects", {

  oop <- new({
    .data <- NULL
    get <- function() { .data }
    set <- function(data) { .data <<- data }
  })

  expect_identical(oop$get(), NULL)
  expect_identical(oop$set(42L), 42L)
  expect_identical(oop$get(), 42L)

  expect_null(oop$data)

  data <- get(".data", envir = oop)
  expect_equal(data, 42L)

})

test_that("rows(), cols() does what we want", {

  indices <- list(
    c(1, 3, 5),
    c(TRUE, FALSE)
  )

  for (index in indices) {

    lhs <- mtcars[index, ]
    rhs <- rows(mtcars, index)
    rownames(lhs) <- rownames(rhs) <- NULL
    expect_equal(lhs, rhs)

    lhs <- mtcars[index]
    rhs <- cols(mtcars, index)
    expect_equal(lhs, rhs)

  }

})

test_that("visited() works as expected", {

  envir <- new.env(parent = emptyenv())
  expect_false(visited("hello", envir))
  expect_true(visited("hello", envir))
  expect_true(visited("hello", envir))

  envir$hello <- FALSE
  expect_false(visited("hello", envir))
  expect_true(visited("hello", envir))

})
