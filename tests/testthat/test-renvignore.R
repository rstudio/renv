
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
