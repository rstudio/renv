
context("State")

test_that("scoped state functions as expected", {

  expect_identical(renv_state$project(), getwd())
  local({
    renv_state$project(tempdir(), scoped = TRUE)
    expect_identical(renv_state$project(), tempdir())
  })
  expect_identical(renv_state$project(), getwd())

})
