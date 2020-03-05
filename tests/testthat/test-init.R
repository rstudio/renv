context("Init")

test_that("we can initialize a project using 'breakfast'", {
  skip_on_cran()
  skip_on_covr()
  renv_tests_scope("breakfast")

  renv::init()
  expect_true(renv_project_initialized(getwd()))

  expected <- c("bread", "breakfast", "oatmeal", "toast")
  lockfile <- renv::snapshot(lockfile = NULL)

  actual <- setdiff(names(renv_records(lockfile)), "renv")
  expect_setequal(actual, expected)

})

test_that("we can initialize a project using 'toast'", {

  skip_on_covr()
  renv_tests_scope("toast")

  renv::init()

  expected <- c("bread", "toast")
  lockfile <- renv::snapshot(lockfile = NULL)

  actual <- setdiff(names(renv_records(lockfile)), "renv")
  expect_setequal(actual, expected)

})

test_that("we cannot initialize a project using 'brunch'", {

  renv_tests_scope("brunch")

  # 'brunch' will fail to install
  renv::init()

  expect_false(file.exists(renv_paths_library("brunch")))

})

test_that("attempts to initialize a project with a missing package is okay", {

  renv_tests_scope("missing")

  # package 'missing' does not exist and so cannot be installed
  renv::init()

  expect_false(file.exists(renv_paths_library("missing")))

})

test_that("the remotes field in a DESCRIPTION is honored", {
  skip_on_cran()

  renv_tests_scope("halloween")
  renv::install("halloween")

  ip <- renv_installed_packages(lib.loc = renv_libpaths_default())
  expect_true("halloween" %in% rownames(ip))
  expect_true("skeleton" %in% rownames(ip))

})

test_that("renv::init(bare = TRUE) initializes a project without packages", {

  renv_tests_scope("brunch")
  renv::init(bare = TRUE)
  files <- list.files(renv_paths_library())
  expect_length(files, 0)

})

test_that("init succeeds even if there are parse errors in project", {

  renv_tests_scope()
  writeLines("oh no", con = "analysis.R")
  renv::init()
  expect_true(file.exists("renv.lock"))

})

test_that("init() restores project containing only a lockfile", {

  renv_tests_scope("breakfast")
  init()

  unlink(".Rprofile")
  unlink("renv", recursive = TRUE)

  restored <- FALSE
  trace(renv:::restore, print = FALSE, function() {
    restored <<- TRUE
  })
  init()
  untrace(renv:::restore)

  expect_true(restored)
  expect_true(file.exists(".Rprofile"))
  expect_true(file.exists("renv/activate.R"))
  expect_true(renv_package_installed("breakfast"))

})
