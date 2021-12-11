
context("Init")

test_that("init() automatically installs referenced packages", {
  skip_on_cran()

  renv_tests_scope("bread")
  init()
  expect_true(renv_package_installed("bread"))

})

test_that("we can initialize a project using 'breakfast'", {
  skip_on_cran()
  skip_on_covr()
  renv_tests_scope("breakfast")

  renv::init()
  expect_true(renv_project_initialized(getwd()))

  expected <- c("bread", "breakfast", "oatmeal", "toast")
  lockfile <- renv::snapshot(lockfile = NULL)

  actual <- setdiff(names(renv_records(lockfile)), "renv")
  expect_setequal(actual, expected)

})

test_that("we can initialize a project using 'toast'", {

  skip_on_covr()
  renv_tests_scope("toast")

  renv::init()

  expected <- c("bread", "toast")
  lockfile <- renv::snapshot(lockfile = NULL)

  actual <- setdiff(names(renv_records(lockfile)), "renv")
  expect_setequal(actual, expected)

})

test_that("we cannot initialize a project using 'brunch'", {

  renv_tests_scope("brunch")

  # 'brunch' will fail to install
  local({
    renv_scope_options(renv.tests.verbose = FALSE)
    renv::init()
  })

  expect_false(file.exists(renv_paths_library("brunch")))

})

test_that("attempts to initialize a project with a missing package is okay", {

  renv_tests_scope("missing")

  # package 'missing' does not exist and so cannot be installed
  local({
    renv_scope_options(renv.tests.verbose = FALSE)
    renv::init()
  })

  expect_false(file.exists(renv_paths_library("missing")))

})

test_that("the remotes field in a DESCRIPTION is honored", {
  skip_on_cran()

  renv_tests_scope("halloween")
  renv::install("halloween")

  ip <- renv_installed_packages(lib.loc = renv_libpaths_default())
  expect_true("halloween" %in% rownames(ip))
  expect_true("skeleton" %in% rownames(ip))

})

test_that("renv::init(bare = TRUE) initializes a project without packages", {

  renv_tests_scope("brunch")
  renv::init(bare = TRUE)
  files <- list.files(renv_paths_library())
  expect_length(files, 0)

})

test_that("init succeeds even if there are parse errors in project", {

  renv_tests_scope()
  writeLines("oh no", con = "analysis.R")

  local({
    renv_scope_options(renv.tests.verbose = FALSE)
    renv::init()
  })

  expect_true(file.exists("renv.lock"))

})

test_that("init() works in path containing accented characters", {

  # ensure the project path can be represented in native encoding
  project <- enc2utf8("pr\u{00f8}ject")

  roundtrip <- tryCatch(
    enc2utf8(enc2native(project)),
    condition = identity
  )

  if (!identical(project, roundtrip))
    skip("project cannot be represented in native encoding")

  native <- enc2native(project)
  renv_tests_scope(project = paste(tempdir(), native, sep = "/"))

  init()

  install("toast")
  expect_true(renv_package_installed("bread"))
  expect_true(renv_package_installed("toast"))

  snapshot(library = paths$library(), type = "all")
  lockfile <- renv_lockfile_load(project = getwd())
  expect_true(!is.null(lockfile$Packages$bread))
  expect_true(!is.null(lockfile$Packages$toast))

})

test_that("we use an external library path for package projects", {

  renv_tests_scope()

  writeLines("Type: Package", con = "DESCRIPTION")
  init()

  library <- renv_libpaths_default()
  userdir <- renv_bootstrap_user_dir()

  expect_true(
    object = renv_path_within(library, userdir),
    info = sprintf("- %s\n- %s\n", library, userdir)
  )

})

test_that("RENV_PATHS_RENV is respected on init", {

  skip_on_cran()

  renv_tests_scope()
  renv_scope_envvars(
    RENV_PATHS_LOCKFILE = ".renv/renv.lock",
    RENV_PATHS_RENV = ".renv"
  )

  local({
    args <- c("-s", "-e", shcode(renv::init()))
    renv_system_exec(R(), args, action = "renv::init()")
    expect_true(file.exists(".renv"))
    expect_true(file.exists(".renv/renv.lock"))
  })

  script <- renv_test_code({
    writeLines(Sys.getenv("RENV_PATHS_RENV"))
  })

  args <- c("-s", "-f", script)
  renv <- local({
    renv_scope_envvars(R_PROFILE_USER = NULL)
    renv_system_exec(R(), args, action = "reading RENV_PATHS_RENV")
  })

  expect_equal(renv, ".renv")

})
