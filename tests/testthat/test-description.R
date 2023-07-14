
test_that("snapshotting broken DESCRIPTION files is an error", {

  # empty file
  file <- renv_scope_tempfile()
  file.create(file)
  expect_error(renv_snapshot_description(file))

  # missing Version field
  fields <- c(Type = "Package", Package = "test")
  renv_dcf_write(fields, file = file)
  expect_error(renv_snapshot_description(file))

})

test_that("we read DESCRIPTION files correctly", {

  contents <- heredoc("
    Package: example
    Description: This is a description.
      Indented fields might have colons: that's fine.
    Depends: apple
    URL: https://posit.co
  ")

  descfile <- renv_scope_tempfile()
  writeLines(contents, con = descfile)
  actual <- renv_description_read(path = descfile)

  expected <- list(
    Package = "example",
    Description = paste(
      "This is a description.",
      "Indented fields might have colons: that's fine."
    ),
    Depends = "apple",
    URL = "https://posit.co"
  )

  expect_equal(actual, expected)

})

test_that("we can read a DESCRIPTION file with empty lines", {

  contents <- heredoc("
    Package: example

    Description: Oops! There's a blank line!
  ")

  descfile <- renv_scope_tempfile()
  writeLines(contents, con = descfile)
  actual <- renv_description_read(path = descfile)

  expected <- list(
    Package = "example",
    Description = "Oops! There's a blank line!"
  )

  expect_equal(actual, expected)

})
