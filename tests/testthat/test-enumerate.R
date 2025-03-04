
test_that("enumerate() handles identity maps", {

  data <- list(
    as.integer(1:10),
    as.numeric(1:10),
    as.logical(1:10),
    letters[1:10]
  )

  for (i in seq_along(data)) {
    expect_identical(
      enumerate(data[[i]], function(key, value) value, FUN.VALUE = data[[i]][[1L]]),
      data[[i]]
    )
  }

})

test_that("enumerate() works as expected", {

  zip <- function(key, value) list(key, value)

  data <- list(a = 1, b = 2, c = 3)
  actual <- enumerate(data, zip)
  expected <- list(a = list("a", 1), b = list("b", 2), c = list("c", 3))
  expect_identical(actual, expected)

  data <- list(a = "1", b = "2", c = "3")
  actual <- enumerate(data, zip)
  expected <- list(a = list("a", "1"), b = list("b", "2"), c = list("c", "3"))
  expect_identical(actual, expected)

  data <- list2env(list(a = "1", b = "2", c = "3"))
  actual <- enumerate(data, zip)
  expected <- list(a = list("a", "1"), b = list("b", "2"), c = list("c", "3"))

  actual <- actual[order(names(actual))]
  expected <- expected[order(names(expected))]
  expect_identical(actual, expected)

})

test_that("enumerate() handles dots", {

  values <- list()
  data <- list(a = 1, b = 2, c = 3)
  enumerate(data, function(key, value, extra) {
    values[[length(values) + 1L]] <<- list(key, value, extra)
  }, extra = TRUE)

  expect_identical(values, list(
    list("a", 1, TRUE),
    list("b", 2, TRUE),
    list("c", 3, TRUE)
  ))

})

test_that("enum_chr() does what it should", {

  actual <- enum_chr(
    list(a = "1", b = "2", c = "3"),
    function(key, value) value
  )

  expected <- c(a = "1", b = "2", c = "3")
  expect_identical(actual, expected)

  actual <- enum_chr(
    list(1, 2, 3),
    function(key, value) as.character(value)
  )

  expected <- c("1", "2", "3")
  expect_identical(actual, expected)

})
