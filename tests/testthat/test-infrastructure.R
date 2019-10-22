
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

test_that("library/ is excluded from .gitignore as appropriate", {

  skip_if_not(nzchar(Sys.which("git")), "git is not available")

  renv_tests_scope()
  system("git init", ignore.stdout = TRUE, ignore.stderr = TRUE)
  renv::init(bare = TRUE)

  contents <- readLines("renv/.gitignore")
  expect_true("library/" %in% contents)

  settings$vcs.ignore.library(FALSE)

  contents <- readLines("renv/.gitignore")
  expect_false("library/" %in% contents)


})
