context("Init")

test_that("we can initialize a project using 'breakfast'", {

  renv_tests_scope("breakfast")

  renv::init()
  expect_true(renv_project_initialized(getwd()))

  expected <- c("bread", "breakfast", "oatmeal", "toast")
  lockfile <- renv::snapshot(lockfile = NULL)
  expect_setequal(names(lockfile$Packages), expected)

})

test_that("we can initialize a project using 'toast'", {

  renv_tests_scope("toast")

  renv::init()

  expected <- c("bread", "toast")
  lockfile <- renv::snapshot(lockfile = NULL)
  expect_setequal(names(lockfile$Packages), expected)

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

test_that("renv::init(bare = TRUE) can be used to initialize a project without installing packages", {

  renv_tests_scope("brunch")
  renv::init(bare = TRUE)
  files <- list.files(renv_paths_library())
  expect_length(files, 0)

})
