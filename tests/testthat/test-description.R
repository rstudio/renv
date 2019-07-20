
context("DESCRIPTION")

test_that("snapshotting broken DESCRIPTION files is an error", {

  file <- tempfile()

  # empty file
  file.create(file)
  expect_s3_class(renv_snapshot_description(file), "error")

  # missing Version field
  fields <- c(Type = "Package", Package = "test")
  renv_dcf_write(fields, file = file)
  expect_s3_class(renv_snapshot_description(file), "error")

})
