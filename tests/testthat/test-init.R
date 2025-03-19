
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

  init()
  expect_true(renv_project_initialized(getwd()))

  expected <- c("bread", "breakfast", "oatmeal", "toast")
  lockfile <- snapshot(lockfile = NULL)

  actual <- setdiff(names(renv_lockfile_records(lockfile)), "renv")
  expect_setequal(actual, expected)

})

test_that("we can initialize a project using 'toast'", {

  skip_on_covr()
  renv_tests_scope("toast")

  init()

  expected <- c("bread", "toast")
  lockfile <- snapshot(lockfile = NULL)

  actual <- setdiff(names(renv_lockfile_records(lockfile)), "renv")
  expect_setequal(actual, expected)

})

test_that("we cannot initialize a project using 'brunch'", {

  renv_tests_scope("brunch")

  # 'brunch' will fail to install
  init()

  expect_false(file.exists(renv_paths_library("brunch")))

})

test_that("attempts to initialize a project with a missing package is okay", {

  renv_tests_scope("missing")

  # package 'missing' does not exist and so cannot be installed
  init()

  expect_false(file.exists(renv_paths_library("missing")))

})

test_that("the remotes field in a DESCRIPTION is honored", {
  skip_on_cran()
  skip_if_no_github_auth()

  renv_tests_scope("halloween")
  install("halloween")

  ip <- installed_packages(lib.loc = renv_libpaths_active())
  expect_true("halloween" %in% ip$Package)
  expect_true("skeleton" %in% ip$Package)

})

test_that("init(bare = TRUE) initializes a project without packages", {

  renv_tests_scope("brunch")
  init(bare = TRUE)
  files <- list.files(renv_paths_library())
  expect_length(files, 0)

})

test_that("init succeeds even if there are parse errors in project", {

  renv_tests_scope()
  writeLines("oh no", con = "analysis.R")

  init()
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

  # TODO(Kevin): Windows tests on CI had the following NOTE
  #
  # Found the following files/directories:
  # 'RtmpKIUpGHprøject'
  #
  # so we should run this down.
  project <- paste(tempdir(), enc2native(project), sep = "/")
  renv_tests_scope(project = project)
  defer(unlink(project, recursive = TRUE))

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

  skip_on_cran()
  renv_tests_scope()

  # use custom userdir
  userdir <- renv_scope_tempfile("renv-userdir-override")
  ensure_directory(userdir)
  userdir <- renv_path_normalize(userdir, mustWork = TRUE)
  renv_scope_options(renv.userdir.override = userdir)

  # sanity check
  userdir <- renv_bootstrap_user_dir()
  if (!identical(expect_true(renv_path_within(userdir, tempdir())), TRUE))
    skip("userdir not in tempdir; cannot proceed")

  # initialize a package project
  writeLines("Type: Package", con = "DESCRIPTION")
  init()

  # check for external library path
  library <- renv_libpaths_active()

  expect_true(
    object = renv_path_within(library, userdir),
    info = sprintf("- %s\n- %s\n", library, userdir)
  )

})

test_that("a project with unnamed repositories can be initialized", {

  skip_on_cran()
  renv_tests_scope()

  repos <- c(
    CRAN = "https://cran.rstudio.com",
    "https://localhost:12345"
  )

  renv_scope_options(repos = repos)
  init()

  lockfile <- renv_lockfile_read("renv.lock")
  repos <- lockfile[["R"]][["Repositories"]]

  expect_equal(
    repos,
    list(
      CRAN = "https://cran.rstudio.com",
      "https://localhost:12345" = "https://localhost:12345"
    )
  )

})

test_that("RENV_PATHS_RENV is respected on init", {

  skip_on_cran()

  renv_tests_scope()
  unlink("renv", recursive = TRUE)

  renv_scope_envvars(
    RENV_PATHS_LOCKFILE = ".renv/renv.lock",
    RENV_PATHS_RENV     = ".renv",
    # don't execute user profile
    R_PROFILE_USER = ""
  )

  # perform init in sub-process
  args <- c("-s", "-e", renv_shell_quote("renv::init()"))
  renv_system_exec(R(), args, action = "executing init()")

  # check that the requisite files were created
  expect_true(file.exists(".renv"))
  expect_true(file.exists(".renv/renv.lock"))

  script <- renv_test_code({
    writeLines(Sys.getenv("RENV_PATHS_RENV"))
  })

  renv_scope_envvars(R_PROFILE_USER = NULL, RENV_PROJECT = NULL)
  args <- c("-s", "-f", script)
  renv <- renv_system_exec(R(), args, action = "reading RENV_PATHS_RENV")

  expect_equal(tail(renv, n = 1L), ".renv")

})

test_that("init() uses PPM by default", {
  skip_on_cran()

  # simulate "fresh" R session with unset repositories
  renv_scope_options(repos = c(CRAN = "@CRAN@"))
  repos <- renv_init_repos()
  expect_equal(repos[["CRAN"]], "https://packagemanager.posit.co/cran/latest")

})

test_that("init() prompts the user for the snapshot type", {
  skip_on_cran()

  project <- renv_tests_scope("bread")
  writeLines("Depends: bread", con = "DESCRIPTION")
  expect_snapshot(init())
  expect_true(renv_package_installed("bread"))

})

test_that("a project can be initialized without loading it", {
  skip_on_cran()

  project <- renv_tests_scope()
  init()
  expect_equal(renv_project_get(), project)

  oldwd <- getwd()
  oldrepos <- getOption("repos")
  oldlibs <- .libPaths()

  other <- renv_scope_tempfile("renv-project-")
  ensure_directory(other)
  init(project = other, load = FALSE, restart = FALSE)
  expect_equal(renv_project_get(), project)
  expect_true(file.exists(file.path(other, "renv.lock")))

  expect_equal(oldwd, getwd())
  expect_equal(oldrepos, getOption("repos"))
  expect_equal(oldlibs, .libPaths())
})

test_that("init() respects user-requested snapshot type", {
  project <- renv_tests_scope()
  writeLines("Depends: bread", con = "DESCRIPTION")
  writeLines("library(toast)", con = "deps.R")
  init()

  expect_true(renv_package_installed("bread"))
  expect_false(renv_package_installed("toast"))
  expect_equal(settings$snapshot.type(), "explicit")
})

test_that("init() respects Remotes in a project DESCRIPTION file", {

  skip_on_cran()
  skip_if_no_github_auth()

  project <- renv_tests_scope("skeleton")
  writeLines("Depends: skeleton\nRemotes: kevinushey/skeleton", con = "DESCRIPTION")
  init()
  expect_true(renv_package_installed("skeleton"))

})

test_that("a project using named remotes can be initialized", {
  project <- renv_tests_scope()

  contents <- heredoc('
    Depends:
      toast
    Remotes:
      toast=toast
  ')
  writeLines(contents, con = "DESCRIPTION")

  init(settings = list(snapshot.type = "explicit"))
  expect_true(renv_package_installed("toast"))

})
