
context("State")

test_that("scoped state functions as expected", {

  expect_false(renv_state$local())
  local({
    renv_state$local(TRUE, scoped = TRUE)
    expect_true(renv_state$local())
  })
  expect_false(renv_state$local())

})
