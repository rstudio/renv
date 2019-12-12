
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
  expect_true("# library/" %in% contents)
  expect_false("library/" %in% contents)

  settings$vcs.ignore.library(TRUE)

  contents <- readLines("renv/.gitignore")
  expect_true("library/" %in% contents)
  expect_false("# library/" %in% contents)

})

test_that("whitespace in infrastructure file is preserved", {
  renv_tests_scope()
  before <- c("^renv$", "", "^renv\\.lock$", "", "c")
  writeLines(before, ".Rbuildignore")
  renv_infrastructure_write()
  after <- readLines(".Rbuildignore")
  expect_equal(before, after)
})

test_that("lines are commented, uncommented as appropriate", {
  renv_tests_scope()

  text <- "  # Some pre-existing text"
  writeLines(text, con = ".Rprofile")

  init(bare = TRUE)

  before <- readLines(".Rprofile")

  deactivate()

  after <- readLines(".Rprofile")

  expected <- c("# source(\"renv/activate.R\")", text)
  expect_identical(after, expected)

})

test_that("comments after a required line are preserved", {
  renv_tests_scope()

  before <- c("# a comment", "source(\"renv/activate.R\")")
  after  <- c("# a comment", "# source(\"renv/activate.R\")")

  writeLines(before, con = ".Rprofile")
  renv_infrastructure_remove()

  expect_identical(readLines(con = ".Rprofile"), after)

  renv_infrastructure_write()
  expect_identical(readLines(con = ".Rprofile"), before)

})

test_that("the project .Rprofile is removed if only autoloader exists", {

  renv_tests_scope()
  writeLines("source(\"renv/activate.R\")", con = ".Rprofile")
  renv_infrastructure_remove()
  expect_false(file.exists(".Rprofile"))

})

test_that("the renv .gitignore is updated for sub-projects", {

  renv_tests_scope()
  dir.create(".git")
  project <- file.path(getwd(), "project")
  dir.create(project)
  renv_infrastructure_write(project = project)
  expect_true(file.exists("project/renv/.gitignore"))

})
