
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

  for (path in abs)
    expect_true(renv_path_absolute(path))

  for (path in rel)
    expect_false(renv_path_absolute(path))

})
