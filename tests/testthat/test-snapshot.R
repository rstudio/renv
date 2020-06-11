
context("Snapshot")

test_that("snapshot is idempotent", {

  renv_tests_scope("oatmeal")

  init(bare = TRUE)
  install("oatmeal")
  snapshot()
  before <- renv_lockfile_read("renv.lock")
  snapshot()
  after <- renv_lockfile_read("renv.lock")
  expect_equal(before, after)

})

test_that("snapshot failures are reported", {

  renv_scope_envvars(RENV_PATHS_ROOT = tempfile())
  renv_tests_scope("oatmeal")
  renv::init()

  descpath <- system.file("DESCRIPTION", package = "oatmeal")
  unlink(descpath)

  output <- tempfile("renv-snapshot-output-")
  local({
    renv_scope_sink(file = output)
    renv::snapshot(prompt = FALSE)
  })

  contents <- readLines(output)
  expect_true(length(contents) > 1)

})

test_that("broken symlinks are reported", {
  skip_on_os("windows")

  renv_scope_envvars(RENV_PATHS_ROOT = tempfile())
  renv_tests_scope("oatmeal")
  renv::init()

  oatmeal <- renv_path_normalize(system.file(package = "oatmeal"), winslash = "/")
  unlink(oatmeal, recursive = TRUE)

  output <- tempfile("renv-snapshot-output-")
  local({
    renv_scope_sink(file = output)
    renv::snapshot(prompt = FALSE)
  })

  contents <- readLines(output)
  expect_true(length(contents) > 1)

})

test_that("multiple libraries can be used when snapshotting", {

  renv_scope_envvars(RENV_PATHS_ROOT = tempfile())
  renv_tests_scope()

  renv::init()

  lib1 <- tempfile("renv-lib1-")
  lib2 <- tempfile("renv-lib2-")
  ensure_directory(c(lib1, lib2))

  oldlibpaths <- .libPaths()
  .libPaths(c(lib1, lib2))

  renv::install("bread", library = lib1)
  breadloc <- find.package("bread")
  expect_true(renv_file_same(dirname(breadloc), lib1))

  renv::install("toast", library = lib2)
  toastloc <- find.package("toast")
  expect_true(renv_file_same(dirname(toastloc), lib2))

  libs <- c(lib1, lib2)
  lockfile <- renv::snapshot(lockfile = NULL, library = libs, type = "all")
  records <- renv_records(lockfile)

  expect_length(records, 2L)
  expect_setequal(names(records), c("bread", "toast"))

  .libPaths(oldlibpaths)

})

test_that("implicit snapshots only include packages currently used", {

  renv_tests_scope("oatmeal")
  renv::init()

  # install toast, but don't declare that we use it
  renv::install("toast")
  lockfile <- snapshot(type = "implicit", lockfile = NULL)
  records <- renv_records(lockfile)
  expect_length(records, 1L)
  expect_setequal(names(records), "oatmeal")

  # use toast
  writeLines("library(toast)", con = "toast.R")
  lockfile <- snapshot(type = "packrat", lockfile = NULL)
  records <- renv_records(lockfile)
  expect_length(records, 3L)
  expect_setequal(names(records), c("oatmeal", "bread", "toast"))

})

test_that("explicit snapshots only capture packages in DESCRIPTION", {

  renv_tests_scope("breakfast")
  renv::init()

  desc <- list(Type = "Project", Depends = "toast")

  write.dcf(desc, file = "DESCRIPTION")
  lockfile <- snapshot(type = "explicit", lockfile = NULL)
  records <- renv_records(lockfile)
  expect_true(length(records) == 2L)
  expect_true(!is.null(records[["bread"]]))
  expect_true(!is.null(records[["toast"]]))

})

test_that("a custom snapshot filter can be used", {
  skip_on_cran()
  renv_tests_scope("breakfast")

  settings$snapshot.type("custom")
  options(renv.snapshot.filter = function(project) c("bread", "toast"))

  renv::init()
  lockfile <- renv_lockfile_load(project = getwd())
  expect_setequal(names(renv_records(lockfile)), c("bread", "toast"))

})

test_that("snapshots that take too long produce a warning", {

  renv_tests_scope("breakfast")
  renv_scope_options(
    renv.config.auto.snapshot = FALSE,
    renv.config.snapshot.filter.timelimit = 0L
  )

  init(bare = TRUE)
  install("breakfast")
  output <- local({
    renv_scope_options(renv.verbose = TRUE)
    capture.output(snapshot(type = "packrat"))
  })

  expect_true(length(output) > 1)

})

test_that("snapshotted packages from CRAN include the Repository field", {

  renv_tests_scope("bread")
  init()

  lockfile <- renv_lockfile_read("renv.lock")
  records <- renv_records(lockfile)
  expect_true(records$bread$Repository == "CRAN")

})

test_that("snapshot failures due to bad library / packages are reported", {

  renv_tests_scope()
  ensure_directory("badlib/badpkg")
  writeLines("invalid", "badlib/badpkg/DESCRIPTION")
  local({
    renv_scope_sink()
    expect_error(snapshot(library = "badlib"))
  })


})

test_that("snapshot ignores own package in package development scenarios", {

  renv_tests_scope()
  ensure_directory("bread")
  setwd("bread")

  writeLines(c("Type: Package", "Package: bread"), con = "DESCRIPTION")

  ensure_directory("R")
  writeLines("function() { library(bread) }", con = "R/deps.R")

  lockfile <- snapshot(lockfile = NULL)
  records <- renv_records(lockfile)
  expect_true(is.null(records[["bread"]]))

})

test_that("snapshot warns about unsatisfied dependencies", {

  renv_tests_scope("toast")
  init(settings = list(use.cache = FALSE))

  descpath <- system.file("DESCRIPTION", package = "toast")
  toast <- renv_description_read(descpath)
  toast$Depends <- "bread (> 1.0.0)"
  renv_dcf_write(toast, file = descpath)

  expect_condition(
    snapshot(),
    class = "renv.snapshot.unsatisfied_dependencies"
  )

})

test_that("snapshot records packages discovered in local sources", {

  renv_tests_scope("skeleton")
  renv_scope_envvars(RENV_PATHS_CACHE = tempfile())

  init(bare = TRUE)

  record <- list(Package = "skeleton", Version = "1.0.1", Source = "Local")
  install(list(record))

  lockfile <- snapshot(lockfile = NULL)
  records <- renv_records(lockfile)
  skeleton <- records[["skeleton"]]

  expect_equal(skeleton$Version, "1.0.1")
  expect_equal(skeleton$Source, "Local")

})

test_that("snapshot prefers RemoteType to biocViews", {

  desc <- list(
    Package = "test",
    Version = "1.0",
    RemoteType = "github",
    biocViews = "Biology"
  )

  descfile <- tempfile()
  renv_dcf_write(desc, file = descfile)
  record <- renv_snapshot_description(descfile)
  expect_identical(record$Source, "GitHub")

})

test_that("parse errors cause snapshot to abort", {
  renv_tests_scope()
  writeLines("parse error", con = "parse-error.R")
  init(bare = TRUE)
  renv_scope_options(renv.config.dependency.errors = "fatal")
  expect_error(snapshot())
})

test_that("records for packages available on other OSes are preserved", {
  skip_on_os("windows")
  renv_tests_scope("unixonly")

  init()

  # fake a windows-only record
  lockfile <- renv_lockfile_read("renv.lock")
  lockfile$Packages$windowsonly <- lockfile$Packages$unixonly
  lockfile$Packages$windowsonly$Package <- "windowsonly"
  lockfile$Packages$windowsonly$Hash <- NULL
  lockfile$Packages$windowsonly$OS_type <- "windows"
  renv_lockfile_write(lockfile, "renv.lock")

  # call snapshot to update lockfile
  snapshot()

  # ensure that 'windowsonly' is still preserved
  lockfile <- renv_lockfile_read("renv.lock")
  expect_true(!is.null(lockfile$Packages$windowsonly))

})

test_that(".renvignore works during snapshot without an explicit root", {

  renv_tests_scope()

  # pretend we don't know the project root
  renv_scope_envvars(RENV_PROJECT = NULL)

  # install bread
  install("bread")

  # create sub-directory that should be ignored
  dir.create("ignored")
  writeLines("library(bread)", con = "ignored/script.R")

  lockfile <- snapshot(project = ".", lockfile = NULL)
  expect_false(is.null(lockfile$Packages$bread))

  writeLines("*", con = "ignored/.renvignore")

  lockfile <- snapshot(project = ".", lockfile = NULL)
  expect_true(is.null(lockfile$Packages$bread))

})
