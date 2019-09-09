
context("Rebuild")

test_that("rebuild forces a package to rebuilt, bypassing cache", {

  renv_tests_scope("bread")
  init()

  records <- local({
    renv_scope_sink()
    renv::rebuild("bread")
  })

  expect_length(records, 1L)

})
