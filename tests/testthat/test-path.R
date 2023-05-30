
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

test_that("renv_path_normalize() normalizes relative paths that don't exist", {

  prefix <- renv_path_normalize(".", mustWork = TRUE)
  path <- "i/dont/exist"

  actual <- renv_path_normalize(path)
  expected <- paste(prefix, "i/dont/exist", sep = "/")
  expect_equal(actual, expected)

})
