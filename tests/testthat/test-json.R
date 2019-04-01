
context("JSON")

test_that("sample JSON strings can be read", {

  expect_identical(
    renv_json_read(text = '[true, false, null]'),
    quote(list(TRUE, FALSE, NA))
  )

  expect_identical(
    renv_json_read(text = '{"{}": "::"}'),
    quote(list("{}" = "::"))
  )

  expect_identical(
    renv_json_read(text = '[{"a": 1.0}, {"b": -1e5}]'),
    quote(list(list(a = 1.0), list(b = -1E5)))
  )


})
