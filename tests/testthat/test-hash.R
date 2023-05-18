
test_that("whitespace does not affect hash", {

  descpath <- renv_tests_path("packages/breakfast/DESCRIPTION")
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

test_that("hash outputs do not change over time", {

  descpath <- file.path(getwd(), "resources/DESCRIPTION")
  hash <- renv_hash_description(descpath)
  expect_equal(hash, "2edf28b7db72297da02d913babfc1ef3")

})
