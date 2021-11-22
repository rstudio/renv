
context("Path")

test_that("renv_path_absolute() reports common absolute paths", {

  abs <- c(
    "/path/to/file",
    "~/path/to/file",
    "C:/path/to/file",
    "C:\\path\\to\\file",
    "\\\\server\\path\\to\\file"
  )

  rel <- c(
    "path/to/file",
    "./path/to/file",
    "../path/to/file",
    "::/weird/path"
  )

  expect_true(all(renv_path_absolute(abs)))
  expect_false(any(renv_path_absolute(rel)))

})
