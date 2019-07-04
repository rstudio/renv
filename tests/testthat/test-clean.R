
context("Clean")

test_that("clean removes stale lockfiles", {

  renv_tests_scope("bread")

  renv::init()
  library <- renv_paths_library()

  # old temporary directory
  tmpdir <- tempfile(tmpdir = library)
  ensure_directory(tmpdir)

  # stale lockfile
  lockpath <- file.path(library, "00LOCK-package")
  ensure_directory(lockpath)
  Sys.setFileTime(lockpath, Sys.time() - 36000)

  # installed but unused package
  local({
    renv_scope_sink()
    suppressWarnings(renv::install("toast"))
  })

  # clean up the project
  renv::clean()

  # check the project has been cleaned
  expect_false(file.exists(tmpdir))
  expect_false(file.exists(lockpath))
  expect_false(file.exists(file.path(library, "toast")))

  # check that toast is not in the cache
  cache <- renv_cache_list()
  expect_false("toast" %in% basename(cache))

})

test_that("clean removes missing projects from the project list", {

  renv_scope_envvars(RENV_PATHS_ROOT = tempfile())
  renv_tests_scope()

  renv::init()

  ensure_directory(renv_paths_root())
  writeLines("/no/such/project", con = renv_paths_root("projects"))

  output <- tempfile("renv-clean-output-")
  local({
    renv_scope_sink(output)
    renv::clean(confirm = FALSE)
  })

  contents <- readLines(renv_paths_root("projects"))
  expect_length(contents, 0)

})

test_that("clean removed unused packages from the cache", {
  skip_on_os("windows")

  renv_scope_envvars(RENV_PATHS_ROOT = tempfile())
  renv_tests_scope()

  renv::init()

  ensure_directory(renv_paths_root())
  renv:::renv_load_project(project = getwd())

  # check that the projects file has been initialized
  projects <- readLines(renv_paths_root("projects"))
  expect_length(projects, 1)
  expect_true(file.exists(projects))

  # install a package (so it enters the cache) but stop using it
  renv::install("oatmeal")
  cachepath <- normalizePath(renv_paths_library("oatmeal"), winslash = "/")

  renv::remove("oatmeal")
  renv::clean(confirm = FALSE)

  expect_false(file.exists(cachepath))

})
