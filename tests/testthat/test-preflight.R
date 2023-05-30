
test_that("snapshot preflight tests catch common issues", {

  renv_tests_scope()

  libpath <- renv_paths_library()
  ensure_parent_directory(libpath)

  # library is a file, not directory
  file.create(libpath)
  expect_error(snapshot(library = libpath))
  unlink(libpath)

  # project library does not exist
  expect_error(snapshot(library = libpath))

  # arbitrary library does not exist
  expect_error(snapshot(library = tempfile()))

})

test_that("renv warns when snapshotting missing dependencies", {
  skip_on_cran()

  project <- renv_tests_scope("breakfast")
  init()

  remove("oatmeal")
  expect_snapshot(snapshot(), error = TRUE)

  lockfile <- renv_lockfile_load(project)
  expect_true(!is.null(lockfile$Packages$oatmeal))

})
