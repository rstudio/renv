
test_that("renv_dots_check only sets bioconductor from bioc is not already set", {

  f <- function(..., bioconductor = NULL) {
    renv_dots_check(...)
    bioconductor
  }

  expect_true(f(bioc = TRUE))
  expect_snapshot(f(bioconductor = FALSE, bioc = TRUE), error = TRUE)

})
