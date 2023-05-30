test_that("deactivate(clean = TRUE) removes all files", {

  renv_tests_scope()
  init()

  deactivate(clean = TRUE)
  expect_false(file.exists("renv.lock"))
  expect_false(file.exists("renv"))
  expect_false(file.exists(".Rprofile"))
})
