
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
  expect_true(NROW(deps) == 0L)

})

test_that("ignore patterns are constructed correctly", {

  # ignore all data directories in project
  expect_equal(
    renv_renvignore_parse_impl("data", "/project"),
    "^\\Q/project/\\E(?:.*/)?\\Qdata\\E(?:/)?$"
  )

  # ignore data directory at root of project
  expect_equal(
    renv_renvignore_parse_impl("/data", "/project"),
    "^\\Q/project/data\\E(?:/)?$"
  )

  # multiple ignores are parsed separately
  expect_equal(
    renv_renvignore_parse_impl(c("/data1", "/data2"), "/project"),
    c(
      "^\\Q/project/data1\\E(?:/)?$",
      "^\\Q/project/data2\\E(?:/)?$"
    )
  )

  # sub-directory ignores are handled
  expect_equal(
    renv_renvignore_parse_impl("data/internal", "/project"),
    "^\\Q/project/data/internal\\E(?:/)?$"
  )

  # negations are handled
  expect_equal(
    renv_renvignore_parse(c("abc.R", "!def.R")),
    list(
      exclude = "^\\Q/\\E(?:.*/)?\\Qabc.R\\E(?:/)?$",
      include = "^\\Q/\\E(?:.*/)?\\Qdef.R\\E(?:/)?$"
    )
  )

})

test_that("empty .renvignore does not ignore anything", {

  renv_tests_scope("oatmeal")
  file.create(".renvignore")
  deps <- dependencies()
  expect_true("oatmeal" %in% deps$Package)

})

test_that("negated .renvignore patterns are handled", {

  renv_tests_scope()
  writeLines(c("script.R", "!script.R"), con = ".renvignore")
  writeLines("library(foo)", con = "script.R")
  deps <- dependencies()
  expect_true("foo" %in% deps$Package)

})

test_that("ignores with a trailing slash are handled", {

  renv_tests_scope()
  writeLines("dir.R/", con = ".renvignore")

  # dir.R is a file, not a directory, so include it
  writeLines("library(foo)", con = "dir.R")
  deps <- dependencies()
  expect_true("foo" %in% deps$Package)

  # dir.R is, in fact, a directory, so ignore it
  unlink("dir.R")
  dir.create("dir.R")
  writeLines("library(bar)", con = "dir.R/deps.R")
  deps <- dependencies()
  expect_false("bar" %in% deps$Package)

  # dotfile exclusions can be overridden
  writeLines(c("dir.R/", "!dir.R/"), con = ".renvignore")
  deps <- dependencies()
  expect_true("bar" %in% deps$Package)

})

test_that(".renvignore can be used to ignore all but certain files", {

  renv_tests_scope()

  writeLines(c("*", "!dependencies.R"), con = ".renvignore")
  writeLines("library(oatmeal)", con = "script.R")
  writeLines("library(bread)", con = "dependencies.R")

  deps <- dependencies()

  expect_true("bread" %in% deps$Package)
  expect_false("oatmeal" %in% deps$Package)

})

test_that("ignores can be set via option if required", {

  renv_tests_scope()

  dir.create("data")
  writeLines("library(A)", con = "data/script.R")

  dir.create("inst")
  writeLines("library(B)", con = "inst/script.R")

  dir.create("ok")
  writeLines("library(C)", con = "ok/script.R")

  exclude <- structure(c("/data/", "/inst/"), asis = TRUE)
  renv_scope_options(renv.renvignore.exclude = exclude)

  deps <- dependencies()
  expect_setequal(deps$Package, "C")

})

test_that("sub-directory inclusion rules are handled properly", {

  renv_tests_scope()

  dir.create("dir/ignored", recursive = TRUE)
  writeLines("library(A)", con = "dir/ignored/script.R")

  dir.create("dir/matched", recursive = TRUE)
  writeLines("library(B)", con = "dir/matched/script.R")

  rules <- heredoc("
    dir/*
    !dir/matched
  ")

  writeLines(rules, con = ".gitignore")

  # system("git init && git add -A && git status")

  deps <- dependencies()
  expect_setequal(deps$Package, "B")

})

test_that(".renvignore supports profile-specific entries", {

  renv_tests_scope()

  ensure_directory("default")
  writeLines("library(default)", con = "default/deps.R")

  ensure_directory("extra")
  writeLines("library(extra)", con = "extra/deps.R")

  contents <- heredoc('

    # ignore all directories by default
    */

    # "extra" profile uses "default" folder
    #| profile == "default"
    !default

    # "extra" profile uses "extra" folder
    #| profile == "extra"
    !extra

  ')

  writeLines(contents, con = ".renvignore")

  renv_scope_envvars(RENV_PROFILE = "default")
  deps <- dependencies(quiet = TRUE)
  expect_equal(deps$Package, "default")

  renv_scope_envvars(RENV_PROFILE = "extra")
  deps <- dependencies(quiet = TRUE)
  expect_equal(deps$Package, "extra")

})
