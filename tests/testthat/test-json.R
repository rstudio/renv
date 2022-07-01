
context("JSON")

test_that("sample JSON strings can be read", {

  expect_identical(
    renv_json_read(text = '[true, false, null]'),
    list(TRUE, FALSE, NULL)
  )

  expect_identical(
    renv_json_read(text = '{"{}": "::"}'),
    list("{}" = "::")
  )

  expect_identical(
    renv_json_read(text = '[{"a": 1.0}, {"b": -1e5}]'),
    list(list(a = 1.0), list(b = -1E5))
  )

  expect_identical(
    renv_json_read(text = '[{}, [], {}]'),
    list(named(list()), list(), named(list()))
  )

  expect_identical(
    renv_json_read(text = '[{"]": "["}]'),
    list(list("]" = "["))
  )

})

test_that("sample R objects can be converted to JSON", {

  before <- list(alpha = 1, beta = 2)
  json <- renv_json_convert(before)
  after <- renv_json_read(text = json)
  expect_equal(before, after)

})

test_that("empty R lists are converted as expected", {

  data <- list()
  json <- renv_json_convert(data)
  expect_equal(json, "[]")

  names(data) <- character()
  json <- renv_json_convert(data)
  expect_equal(json, "{}")

})

test_that("we can parse a GitHub remotes specification", {

  skip_on_cran()
  skip_sometimes()

  data <- renv_remotes_resolve("rstudio/renv")
  expect_true(data$Source == "GitHub")
  expect_true(data$RemoteUsername == "rstudio")
  expect_true(data$RemoteRepo == "renv")

})

test_that("we can parse a GitHub remotes specification with 'wininet'", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not(renv_platform_windows())

  renv_scope_envvars(RENV_DOWNLOAD_FILE_METHOD = "wininet")
  data <- renv_remotes_resolve("rstudio/renv")
  expect_true(data$Source == "GitHub")
  expect_true(data$RemoteUsername == "rstudio")
  expect_true(data$RemoteRepo == "renv")

})

test_that("we can read json containing escape characters", {
  actual <- list(data = "\\\"")
  json <- renv_json_convert(actual)
  expected <- renv_json_read(text = json)
  expect_equal(actual, expected)
})

test_that("JSON null is read as R NULL", {
  json <- "{\"NULL\": null}"
  actual <- renv_json_read(text = json)
  expected <- list("NULL" = NULL)
  expect_equal(actual, expected)
})

test_that("some common control characters are escaped", {
  json <- renv_json_convert("\b\f\n\r\t")
  expect_equal(json, "\"\\b\\f\\n\\r\\t\"")
})

test_that("scalar values are boxed if requested", {

  json <- renv_json_convert(
    object = list(A = "hello", B = "world"),
    config = list(box = "B")
  )

  value <- renv_json_read(text = json)
  expect_equal(value, list(A = "hello", B = list("world")))

})

test_that("the renv + jsonlite JSON readers are compatible", {

  skip_if_not_installed("jsonlite")

  renv_tests_scope("breakfast")
  init()

  lhs <- renv_json_read_default("renv.lock")
  rhs <- renv_json_read_jsonlite("renv.lock")
  expect_equal(lhs, rhs)

})
