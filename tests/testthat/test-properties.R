
context("Properties")

test_that("properties can be read successfully", {

  data <- '# This is a comment.\nKey: Value'

  path <- renv_tempfile("renv-properties-")
  writeLines(data, con = path)

  # trim whitespace by default
  props <- renv_properties_read(path = path)
  expect_identical(props, list(Key = "Value"))

  # without trimming whitespace
  props <- renv_properties_read(path = path, trim = FALSE)
  expect_identical(props, list(Key = " Value"))

})

test_that("quoted properties are unquoted", {

  text <- "Key=\"Value\""

  props <- renv_properties_read(
    text      = text,
    delimiter = "=",
    dequote   = TRUE
  )

  expect_identical(props, list(Key = "Value"))

})
