
test_that("plan() creates a lockfile without installing packages", {

  project <- renv_tests_scope()
  init()

  renv_tests_dependencies("breakfast")
  lockfile <- plan(project = project)

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

test_that("plan() with explicit packages resolves dependencies", {

  project <- renv_tests_scope()
  init()

  lockfile <- plan(packages = "toast", project = project)
  records <- renv_lockfile_records(lockfile)

  expect_true("toast" %in% names(records))
  expect_true("bread" %in% names(records))
  expect_false("breakfast" %in% names(records))

})

test_that("plan() reports up-to-date when lockfile matches", {

  project <- renv_tests_scope()
  init()

  renv_tests_dependencies("breakfast")

  # run plan twice -- second time should signal up-to-date condition
  plan(project = project)
  expect_condition(
    plan(project = project),
    class = "renv.plan.up_to_date"
  )

})
