
test_that("clean removes stale lockfiles", {

  renv_tests_scope("bread")

  init()
  library <- renv_paths_library()

  # old temporary directory
  tmpdir <- tempfile(tmpdir = library)
  ensure_directory(tmpdir)

  # stale lockfile
  lockpath <- file.path(library, "00LOCK-package")
  ensure_directory(lockpath)
  Sys.setFileTime(lockpath, Sys.time() - 36000)

  # installed but unused package
  suppressWarnings(install("toast"))

  # clean up the project
  actions <- c("package.locks", "library.tempdirs", "unused.packages")
  clean(actions = actions)

  # check the project has been cleaned
  expect_false(file.exists(tmpdir))
  expect_false(file.exists(lockpath))
  expect_false(file.exists(file.path(library, "toast")))

})
