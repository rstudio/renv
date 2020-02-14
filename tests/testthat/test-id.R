
context("ID")

test_that("we can generate a unique ID", {
  skip_on_cran()
  id <- renv_id_generate()
  expect_true(nchar(id) == 36L)
  expect_true(grepl("^[a-fA-F0-9-]+$", id))
})

test_that("projects are assigned a unique ID when created", {
  skip_on_cran()
  renv_tests_scope()

  renv::init(bare = TRUE)
  expect_true(file.exists("renv/project-id"))

  contents <- readLines("renv/project-id")
  expect_identical(contents, renv_project_id(getwd()))

  id <- renv_project_id(getwd())
  contents <- readLines("renv/project-id")

})
