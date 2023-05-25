
test_that("status reports packages to be installed / changed", {

  renv_tests_scope(c("toast", "breakfast"))
  renv_scope_options(renv.config.auto.snapshot = FALSE)

  init(bare = TRUE)
  expect_snapshot({
    snapshot()
  })

  install("breakfast")
  expect_snapshot({
    snapshot()
  })

  record("egg")
  expect_snapshot({
    snapshot()
  })

})

test_that("status reports packages which are used but not installed", {

  renv_tests_scope()
  init()

  writeLines("library(bread)", con = "script.R")
  expect_snapshot(
    status()
  )

})
