
context("renvignore")

test_that(".renvignore ignores files, directories", {

  renv_tests_scope("oatmeal")

  writeLines(c("internal/", "data.R"), con = ".renvignore")

  # data file at root
  writeLines("library(breakfast)", con = "data.R")

  # internal folder at root
  dir.create("internal")
  writeLines("library(breakfast)", con = "internal/script.R")

  # internal subfolder
  dir.create("scripts/internal", recursive = TRUE)
  writeLines("library(breakfast)", con = "scripts/internal/script.R")

  deps <- dependencies(root = getwd())
  expect_setequal(deps$Package, "oatmeal")

  deps <- dependencies("scripts", root = getwd())
  expect_true(NROW(deps) == 0)

})

test_that("ignore patterns are constructed correctly", {

  # ignore all data directories in project
  expect_equal(
    renv_renvignore_parse_impl("data", "/project"),
    "^\\Q/project/\\E(?:(?:.*/)?\\Qdata\\E$)"
  )

  # ignore data directory at root of project
  expect_equal(
    renv_renvignore_parse_impl("/data", "/project"),
    "^\\Q/project/\\E(?:\\Qdata\\E$)"
  )

  # ignores are combined correctly
  expect_equal(
    renv_renvignore_parse_impl(c("/data1", "/data2"), "/project"),
    "^\\Q/project/\\E(?:\\Qdata1\\E$|\\Qdata2\\E$)"
  )

  expect_equal(
    renv_renvignore_parse_impl(c("data1", "data2"), "/project"),
    "^\\Q/project/\\E(?:(?:.*/)?\\Qdata1\\E$|(?:.*/)?\\Qdata2\\E$)"
  )

  # sub-directory ignores are handled
  expect_equal(
    renv_renvignore_parse_impl("data/internal", "/project"),
    "^\\Q/project/\\E(?:\\Qdata/internal\\E$)"
  )

  # negations are handled
  expect_equal(
    renv_renvignore_parse(c("abc.R", "!def.R")),
    list(
      exclude = "^\\Q/\\E(?:(?:.*/)?\\Qabc.R\\E$)",
      include = "^\\Q/\\E(?:(?:.*/)?\\Qdef.R\\E$)"
    )
  )

})

test_that("empty .renvignore does not ignore anything", {

  renv_tests_scope("oatmeal")
  file.create(".renvignore")
  deps <- dependencies(quiet = TRUE)
  expect_true("oatmeal" %in% deps$Package)

})

test_that("negated .renvignore patterns are handled", {

  renv_tests_scope()
  writeLines(c("script.R", "!script.R"), con = ".renvignore")
  writeLines("library(foo)", con = "script.R")
  deps <- dependencies(quiet = TRUE)
  expect_true("foo" %in% deps$Package)

})
