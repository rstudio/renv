
test_that("we can clean up empty directory hierarchies", {

  root <- renv_scope_tempfile("renv-root-")
  ensure_directory(root)
  renv_scope_wd(root)

  dir.create("A/B", recursive = TRUE)
  dir.create("C", recursive = TRUE)
  dir.create("D")
  file.create("D/E")
  file.create("F")

  actual <- list.files(recursive = TRUE, include.dirs = TRUE)
  expected <- c("A", "A/B", "C", "D", "D/E", "F")
  expect_equal(sort(actual), sort(expected))

  renv_cleanse_empty(path = getwd())

  actual <- list.files(recursive = TRUE, include.dirs = TRUE)
  expected <- c("D", "D/E", "F")
  expect_equal(sort(actual), sort(expected))

})
