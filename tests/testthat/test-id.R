
context("ID")

test_that("we can generate a unique ID", {
  skip_on_cran()
  id <- renv_id_generate()
  expect_true(nchar(id) == 36L)
  expect_true(grepl("^[a-fA-F0-9-]+$", id))
})
