
test_that("we can check out packages from our local repository", {

  # enter test scope
  project <- renv_tests_scope()
  init()

  # check out a package + its dependencies; this invocation is
  # similar in spirit to a plain `install()` call
  renv_tests_dependencies("breakfast")
  checkout(packages = "breakfast")

  # check that they were installed
  expect_true(renv_package_installed("breakfast"))
  expect_true(renv_package_installed("bread"))

})

test_that("checkout with actions = 'snapshot' creates a lockfile without installing", {

  project <- renv_tests_scope()
  init()

  # checkout with snapshot action only -- no packages should be installed
  checkout(packages = "breakfast", actions = "snapshot")

  # packages should not be installed
  expect_false(renv_package_installed("breakfast"))
  expect_false(renv_package_installed("bread"))

  # lockfile should exist and contain the full dependency tree
  lockfile <- renv_lockfile_read(paths$lockfile(project = project))
  records <- renv_lockfile_records(lockfile)

  expect_true("breakfast" %in% names(records))
  expect_true("oatmeal" %in% names(records))
  expect_true("toast" %in% names(records))
  expect_true("bread" %in% names(records))

  # records should have dependency metadata
  expect_identical(records$breakfast$Source, "Repository")
  expect_identical(records$breakfast$Version, "1.0.0")
  expect_true(!is.null(records$breakfast$Depends))

})

test_that("activate.R has no unreplaced placeholders after checkout", {

  project <- renv_tests_scope("breakfast")
  init()

  checkout(packages = "breakfast", actions = c("snapshot", "restore"))

  activate <- renv_paths_activate(project = project)
  contents <- readLines(activate)
  placeholders <- grep("\\.\\.[a-zA-Z0-9]+\\.\\.", contents, value = TRUE)
  expect_length(placeholders, 0)

})

test_that("renv_infrastructure_write_activate is skipped during checkout", {

  project <- renv_tests_scope("breakfast")
  init()

  renv_scope_binding(the, "activate_deferred", TRUE)
  result <- renv_infrastructure_write_activate(project = project)
  expect_false(result)

})

test_that("we can check out packages from the package manager instance", {

  skip_on_cran()

  renv_tests_scope()
  init()

  # ensure we reset repos on exit
  renv_scope_options(repos = getOption("repos"))

  # install renv from an old snapshot (pure R, no compiled code)
  . <- checkout(date = "2023-01-02", packages = "renv")

})
