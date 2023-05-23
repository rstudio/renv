
test_that("an existing lockfile can be updated", {

  renv_tests_scope("bread")
  init()

  # change bread to 0.1.0
  record(list(bread = "bread@0.1.0"))

  lockfile <- renv_lockfile_read("renv.lock")
  records <- renv_lockfile_records(lockfile)
  expect_length(records, 1L)
  expect_equal(records$bread$Version, "0.1.0")

  # add a new record
  record(list(toast = "toast@1.0.0"))

  lockfile <- renv_lockfile_read("renv.lock")
  records <- renv_lockfile_records(lockfile)
  expect_length(records, 2L)
  expect_equal(records$bread$Version, "0.1.0")
  expect_equal(records$toast$Version, "1.0.0")

  # use short-hand
  lockfile <- record("toast@1.0.1", lockfile = lockfile)
  records <- renv_lockfile_records(lockfile)
  expect_equal(records$toast$Version, "1.0.1")

  lockfile <- record(list(toast = "1.0.2"), lockfile = lockfile)
  records <- renv_lockfile_records(lockfile)
  expect_equal(records$toast$Version, "1.0.2")

  # remove a record
  lockfile <- record(list(toast = NULL), lockfile = lockfile)
  records <- renv_lockfile_records(lockfile)
  expect_true(is.null(records$toast))

})

test_that("record(<package>) also records version", {

  renv_tests_scope()

  # create empty lockfile
  snapshot()

  # add a record
  record("breakfast")

  # check that the version of breakfast was recorded
  lockfile <- renv_lockfile_load(project = getwd())
  breakfast <- lockfile$Packages$breakfast
  expect_identical(breakfast$Version, "1.0.0")

})
