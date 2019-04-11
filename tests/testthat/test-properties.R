
context("Properties")

test_that("properties can be read successfully", {

  data <- '# This is a comment.\nKey: Value'

  path <- renv_tempfile("renv-properties-")
  writeLines(data, con = path)

  # trim whitespace by default
  props <- renv_read_properties(path = path)
  expect_identical(props, list(Key = "Value"))

  # without trimming whitespace
  props <- renv_read_properties(path = path, trim = FALSE)
  expect_identical(props, list(Key = " Value"))

})
