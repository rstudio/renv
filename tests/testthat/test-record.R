
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

test_that("record() writes the same fields as snapshot() for an installed package", {

  # https://github.com/rstudio/rsconnect/issues/1319
  # record() resolves a remote (Package/Version/Source/Repository) but does not
  # read the installed DESCRIPTION, so it produces a thinner record than
  # snapshot() does for the same package -- missing Hash, Requirements, and
  # the dependency fields that downstream tools (e.g. rsconnect) rely on.

  renv_tests_scope("breakfast")
  init()

  # capture the record snapshot() (via init) wrote
  lockfile <- renv_lockfile_read("renv.lock")
  snap_record <- renv_lockfile_records(lockfile)$breakfast

  # overwrite the same package via record()
  record("breakfast")

  lockfile <- renv_lockfile_read("renv.lock")
  rec_record <- renv_lockfile_records(lockfile)$breakfast

  # both records describe the same installed package, so they should carry
  # the same fields
  expect_setequal(names(rec_record), names(snap_record))

})
