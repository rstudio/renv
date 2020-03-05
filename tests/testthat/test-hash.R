
context("Hash")

test_that("whitespace does not affect hash", {

  descpath <- file.path(renv_tests_root(), "packages/breakfast/DESCRIPTION")
  contents <- readLines(descpath)
  hash <- renv_hash_description(descpath)

  # NOTE: this also implies that the hash should be stable
  # across different versions of R
  expect_identical(hash, "0fcd2a795901b4b21326a3e35442c97c")

  renv_scope_tempdir()
  descpath <- file.path(getwd(), "DESCRIPTION")

  for (whitespace in c(" ", "\t")) {
    writeLines(paste(contents, whitespace), descpath)
    expect_identical(renv_hash_description(descpath), hash)
  }

})
