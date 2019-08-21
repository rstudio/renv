
context("Infrastructure")

test_that("renv.lock is added to .Rbuildignore", {

  renv_tests_scope()
  writeLines(c("Type: Package", "Package: test"), con = "DESCRIPTION")
  renv::init()

  expect_true(file.exists(".Rbuildignore"))
  contents <- readLines(".Rbuildignore")
  expect_true("^renv\\.lock$" %in% contents)

})

test_that("infrastructure can be removed", {

  renv_tests_scope("breakfast")

  before <- list.files(recursive = TRUE)
  init()
  deactivate()
  renv_infrastructure_remove()
  unlink("renv.lock")
  after <- list.files(recursive = TRUE)

  expect_setequal(before, after)

})
