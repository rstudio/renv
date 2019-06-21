
context("Archives")

test_that("attempts to decompress invalid archives cause R errors to be reported", {

  badtar <- tempfile(fileext = ".tar")
  writeLines("oh no", con = badtar)
  expect_error(renv_archive_decompress(badtar, verbose = TRUE))

  badzip <- tempfile(fileext = ".zip")
  writeLines("oh no", con = badzip)
  expect_error(renv_archive_decompress(badzip))

})

test_that("we can successfully compress / decompress some sample files", {

  dir <- tempfile()
  ensure_directory(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  owd <- setwd(dir)
  on.exit(setwd(owd), add = TRUE)

  for (letter in letters)
    writeLines(letter, con = letter)

  tarfile <- tempfile(fileext = ".tar.gz")
  tar(tarfile, files = ".")

  actual <- list.files(dir)
  expected <- basename(renv_archive_list(tarfile))
  expect_setequal(actual, expected)

  exdir <- tempfile()
  renv_archive_decompress(tarfile, exdir = exdir)
  expect_setequal(list.files(exdir), list.files(dir))


  zipfile <- tempfile(fileext = ".zip")
  zip(zipfile, files = ".", extras = "-q")

  actual <- list.files(dir)
  expected <- basename(renv_archive_list(zipfile))
  expect_setequal(actual, expected)

  exdir <- tempfile()
  renv_archive_decompress(zipfile, exdir = exdir)
  expect_setequal(list.files(exdir), list.files(dir))

})
