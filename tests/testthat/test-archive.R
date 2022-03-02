
context("Archives")

test_that("renv reports errors when decompressing invalid archives", {

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
  expected <- setdiff(basename(renv_archive_list(tarfile)), ".")
  expect_setequal(actual, expected)

  exdir <- tempfile()
  renv_archive_decompress(tarfile, exdir = exdir)
  expect_setequal(list.files(exdir), list.files(dir))

  zipper <- Sys.getenv("R_ZIPCMD", unset = "zip")
  if (nzchar(Sys.which(zipper))) {

    zipfile <- tempfile(fileext = ".zip")
    zip(zipfile, files = ".", extras = "-q")

    actual <- list.files(dir)
    expected <- basename(renv_archive_list(zipfile))
    expect_setequal(actual, expected)

    exdir <- tempfile()
    renv_archive_decompress(zipfile, exdir = exdir)
    expect_setequal(list.files(exdir), list.files(dir))

  }

})

test_that("we can decompress an archive with a tilde path", {
  skip_on_cran()
  skip_on_windows()

  renv_tests_scope()
  renv_scope_envvars(HOME = getwd())
  renv_scope_envvars(tar = Sys.which("tar"))

  # check that we actually set home correctly?
  if (!identical(path.expand("~"), getwd()))
    skip("couldn't override home path")

  # NOTE: in older versions of R, only paths to directory were accepted,
  # so we run out test by attempting to tar up a directory rather than file
  dir.create("subdir")
  writeLines("hello", con = "subdir/a.txt")
  writeLines("goodbye", con = "subdir/b.txt")
  tar("files.tar.gz", files = "subdir", compression = "gzip")
  unlink("subdir", recursive = TRUE)

  # double check the files we have in the archive
  # (renv_archive_list might report the folder itself so test files individually)
  files <- renv_archive_list("files.tar.gz")
  expect_true("subdir/a.txt" %in% files)
  expect_true("subdir/b.txt" %in% files)
  expect_false(file.exists("subdir/a.txt"))
  expect_false(file.exists("subdir/b.txt"))

  archive <- "~/files.tar.gz"
  if (!file.exists(archive))
    skip("archive does not exist in home directory")

  renv_archive_decompress(
    archive = archive,
    exdir   = getwd()
  )

  expect_true(file.exists("subdir/a.txt"))
  expect_true(file.exists("subdir/b.txt"))

})
