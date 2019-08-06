
context("Infrastructure")

test_that("renv.lock is added to .Rbuildignore", {

  renv_tests_scope()
  writeLines(c("Type: Package", "Package: test"), con = "DESCRIPTION")
  renv::init()

  expect_true(file.exists(".Rbuildignore"))
  contents <- readLines(".Rbuildignore")
  expect_true("^renv\\.lock$" %in% contents)

})
