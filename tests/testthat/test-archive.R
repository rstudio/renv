
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

  etc <- file.path(R.home("etc"))
  owd <- setwd(etc)
  on.exit(setwd(owd), add = TRUE)

  tarfile <- tempfile(fileext = ".tar.gz")
  tar(tarfile, files = ".")
  expect_setequal(list.files(etc), basename(renv_archive_list(tarfile)))

  exdir <- tempfile()
  renv_archive_decompress(tarfile, exdir = exdir)

  expect_setequal(list.files(exdir), list.files(etc))


  zipfile <- tempfile(fileext = ".zip")
  zip(zipfile, files = ".", extras = "-q")
  expect_setequal(list.files(etc), basename(renv_archive_list(zipfile)))

  exdir <- tempfile()
  renv_archive_decompress(zipfile, exdir = exdir)

  expect_setequal(list.files(exdir), list.files(etc))

})
