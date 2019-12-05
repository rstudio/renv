
context("Record")

test_that("an existing lockfile can be updated", {

  renv_tests_scope("bread")
  renv::init()

  # change bread to 0.1.0
  renv::record(list(bread = "bread@0.1.0"))

  lockfile <- renv_lockfile_read("renv.lock")
  records <- renv_records(lockfile)
  expect_length(records, 1L)
  expect_equal(records$bread$Version, "0.1.0")

  # add a new record
  renv::record(list(toast = "toast@1.0.0"))

  lockfile <- renv_lockfile_read("renv.lock")
  records <- renv_records(lockfile)
  expect_length(records, 2L)
  expect_equal(records$bread$Version, "0.1.0")
  expect_equal(records$toast$Version, "1.0.0")

  # use short-hand
  lockfile <- renv::record("toast@1.0.1", lockfile = lockfile)
  records <- renv_records(lockfile)
  expect_equal(records$toast$Version, "1.0.1")

  lockfile <- renv::record(list(toast = "1.0.2"), lockfile = lockfile)
  records <- renv_records(lockfile)
  expect_equal(records$toast$Version, "1.0.2")

  # remove a record
  lockfile <- renv::record(list(toast = NULL), lockfile = lockfile)
  records <- renv_records(lockfile)
  expect_true(is.null(records$toast))

})
