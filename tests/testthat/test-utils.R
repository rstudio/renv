
context("Utils")

test_that("common utils work as expected", {
  expect_equal(NULL %NULL% 42, 42)
  expect_equal(lines(1, 2, 3), "1\n2\n3")

  if (nzchar(Sys.which("git")))
    expect_equal(git(), Sys.which("git"))
  else
    expect_error(git())
})

test_that("bind_list handles named lists", {

  data <- list(
    alpha = list(A = 1, B = 2),
    beta  = list(A = 3, B = 4),
    gamma = list(A = 5, B = 6)
  )

  actual <- bind_list(data)
  expected <- data.frame(
    Index = names(data),
    A = c(1, 3, 5),
    B = c(2, 4, 6),
    stringsAsFactors = FALSE
  )

  expect_equal(actual, expected)

})

test_that("bind_list warns on name collision", {
  data <- list(alpha = list(Index = 1), beta = list(Index = 2))
  expect_error(bind_list(data))
})

test_that("bind_list() handles data.frames with potentially different names", {

  data <- list(
    a = data.frame(A = 1, B = 2),
    b = data.frame(A = 1, C = 3)
  )

  bound <- bind_list(data)
  expected <- data.frame(
    Index = c("a", "b"),
    A     = c(1, 1),
    B     = c(2, NA),
    C     = c(NA, 3),
    stringsAsFactors = FALSE
  )

  expect_identical(bound, expected)

})

test_that("bind_list() preserves order where possible", {

  data <- list(
    a = data.frame(A = 1,        C = 3),
    b = data.frame(A = 1, B = 2, C = 3)
  )

  bound <- bind_list(data)
  expect_equal(names(bound), c("Index", "A", "B", "C"))

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

test_that("aliased_path() correctly forms aliased path", {
  path <- "~/some/path"
  expanded <- path.expand(path)
  expect_equal(path, aliased_path(expanded))
})

test_that("memoize avoids evaluating expression multiple times", {

  envir <- new.env(parent = emptyenv())
  key <- "test"

  value <- 0
  memoize(key, { value <- value + 1 }, envir)
  memoize(key, { value <- value + 1 }, envir)

  expect_equal(envir$test, 1)
  expect_equal(value, 1)

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
