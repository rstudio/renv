
test_that("we can query actions for a sample project", {

  project <- renv_tests_scope("breakfast")
  renv_scope_options(renv.config.auto.snapshot = FALSE)
  init(bare = TRUE)
  install("breakfast")

  # project depends on 'breakfast' and 'breakfast' is installed
  # ergo, need to snapshot 4 packages
  acts <- actions("snapshot", project = getwd())
  expect_equal(nrow(acts), 4L)
  expect_setequal(acts$Package, c("bread", "breakfast", "oatmeal", "toast"))
  expect_true(all(acts$Action == "install"))

  # note: empty for non-clean restore as we don't remove packages
  acts <- actions("restore", library = renv_paths_library(), project = getwd())
  expect_equal(nrow(acts), 0L)

  # now non-empty
  acts <- actions("restore", library = renv_paths_library(), project = getwd(), clean = TRUE)
  expect_equal(nrow(acts), 4L)
  expect_setequal(acts$Package, c("bread", "breakfast", "oatmeal", "toast"))
  expect_true(all(acts$Action == "remove"))

})

test_that("we can query actions when no lockfile has yet been generated", {
  renv_tests_scope("bread")
  init(bare = TRUE)
  install("bread")
  actions <- actions("snapshot")
  expect_equal(nrow(actions), 1L)
})

test_that("bare usages of actions work as expected", {

  # set up project with 3 packages that need to be snapshotted
  renv_tests_scope("bread")
  renv_scope_options(renv.config.auto.snapshot = FALSE)

  init()
  install("breakfast")

  writeLines("library(breakfast)", con = "deps.R")
  acts <- actions("snapshot")

  expect_equal(nrow(acts), 3L)
  expect_setequal(acts$Package, c("breakfast", "oatmeal", "toast"))
  expect_true(all(acts$Action == "install"))

})
