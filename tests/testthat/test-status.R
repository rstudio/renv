
test_that("reports if status not possible", {

  renv_tests_scope()
  expect_snapshot(. <- status())

  init(bare = TRUE)
  expect_snapshot(. <- status())

  snapshot()
  unlink("renv/library", recursive = TRUE)
  expect_snapshot(. <- status())

})

test_that("reports when project is synchronised", {

  renv_tests_scope()
  init()

  expect_snapshot(. <- status())

})

test_that("reports installation problems with non-installed packages", {

  renv_tests_scope()
  init()

  writeLines(c("library(egg)", "library(bread)"), con = "script.R")
  record("egg")
  record("oatmeal")

  expect_snapshot(. <- status())

})

test_that("reports synchronisation problems with installed packages", {

  renv_tests_scope()
  init()
  install(c("egg", "bread"))

  writeLines("library(bread)", con = "script.R")
  record("egg")

  expect_snapshot(. <- status())

})

test_that("reports version differences", {

  renv_tests_scope(c("egg", "oatmeal"))
  init()
  record("egg@2.0.0")
  record("oatmeal@0.9.0")

  expect_snapshot(. <- status())

})

test_that("status() notifies user if R version does not match", {

  project <- renv_tests_scope()
  init()

  # simulate a different version of R in lockfile
  local({
    renv_scope_options(renv.verbose = FALSE)
    lockfile <- renv_lockfile_read(file = "renv.lock")
    lockfile$R$Version <- "1.0.0"
    renv_lockfile_write(lockfile, file = "renv.lock")
  })

  expect_snapshot(. <- status())

})

test_that("status() notifies user if packages are missing and inconsistent", {

  project <- renv_tests_scope("bread")
  init()

  writeLines("library(breakfast)", con = "_deps.R")
  install("bread@0.1.0")

  expect_snapshot(. <- status())

})

test_that("status() version check can be disabled", {

  project <- renv_tests_scope("bread")
  init()

  # write an incompatible R version into the lockfile
  lockfile <- renv_lockfile_read(file = "renv.lock")
  lockfile$R$Version <- "1.0.0"
  renv_lockfile_write(lockfile, file = "renv.lock")

  renv_scope_options(renv.status.check_version = TRUE)
  expect_snapshot(. <- renv::status())

  renv_scope_options(renv.status.check_version = FALSE)
  expect_snapshot(. <- renv::status())

})

test_that("status() uses settings$snapshot.dev() as default for dev parameter", {

  renv_tests_scope()
  init()

  # Create a project that would have dev dependencies
  writeLines("PackageUseDevtools: Yes", con = "project.Rproj")

  project <- getwd()

  # Verify default setting is FALSE
  expect_false(settings$snapshot.dev())

  # Test that the setting can be set and retrieved
  settings$snapshot.dev(TRUE)
  expect_true(settings$snapshot.dev())

  settings$snapshot.dev(FALSE)
  expect_false(settings$snapshot.dev())

  # Test that dependencies discovered with dev=FALSE don't include devtools
  deps1 <- renv_snapshot_dependencies(project, dev = FALSE)
  expect_false("devtools" %in% deps1)

  # Test that dependencies discovered with dev=TRUE do include devtools
  deps2 <- renv_snapshot_dependencies(project, dev = TRUE)
  expect_true("devtools" %in% deps2)

})
