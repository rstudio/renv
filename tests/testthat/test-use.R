
context("Use")

test_that("use() works as intended", {

  renv_tests_scope()
  init()

  oldpaths <- .libPaths()
  use("toast", isolate = FALSE, attach = FALSE)
  newpaths <- .libPaths()

  expect_true(length(newpaths) == length(oldpaths) + 1)

  toast <- find.package("toast")
  expect_true(renv_file_same(dirname(toast), .libPaths()[1]))

})
