context("Init")

test_that("we can initialize a project using 'breakfast'", {

  renv_tests_scope("breakfast")

  renv::init()
  expect_true(renv_project_initialized(getwd()))

  expected <- c("bread", "breakfast", "oatmeal", "toast")
  lockfile <- renv::snapshot(lockfile = NULL)
  expect_setequal(names(lockfile$R$Package), expected)

})

test_that("we can initialize a project using 'toast'", {

  renv_tests_scope("toast")

  renv::init()

  expected <- c("bread", "toast")
  lockfile <- renv::snapshot(lockfile = NULL)
  expect_setequal(names(lockfile$R$Package), expected)

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

test_that("use.cache project setting is honored", {
  skip_on_os("windows")

  renv_tests_scope("breakfast")

  renv::init()

  packages <- list.files(renv_paths_library(), full.names = TRUE)
  types <- renv_file_type(packages)
  expect_true(all(types == "symlink"))

  renv::settings$use.cache(FALSE)

  packages <- list.files(renv_paths_library(), full.names = TRUE)
  types <- renv_file_type(packages)
  expect_true(all(types == "directory"))

  renv::settings$use.cache(TRUE)

  packages <- list.files(renv_paths_library(), full.names = TRUE)
  types <- renv_file_type(packages)
  expect_true(all(types == "symlink"))

})

test_that("the remotes field in a DESCRIPTION is honored", {
  skip_on_cran()

  renv_tests_scope("halloween")
  renv::install("halloween")

  ip <- renv_installed_packages(lib.loc = renv_libpaths_default())
  expect_true("halloween" %in% rownames(ip))
  expect_true("skeleton" %in% rownames(ip))

})
