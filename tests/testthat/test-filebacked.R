
context("Filebacked")

test_that("filebacked entries are discarded after the file is modified", {

  file <- renv_scope_tempfile("renv-test-")

  contents <- "Hello, world!"
  writeLines(contents, con = file)

  renv_filebacked_set("test", file, contents)
  expect_equal(renv_filebacked_get("test", file), contents)

  writeLines("Goodbye, world!", con = file)
  expect_identical(renv_filebacked_get("test", file), NULL)

})

test_that("filebacked entries are discarded after the file is deleted", {

  file <- renv_scope_tempfile("renv-test-")

  contents <- "Hello, world!"
  writeLines(contents, con = file)

  renv_filebacked_set("test", file, contents)
  expect_equal(renv_filebacked_get("test", file), contents)

  unlink(file)
  expect_identical(renv_filebacked_get("test", file), NULL)

})
