
test_that("renv/profile is read and used to select a profile", {
  skip_on_cran()

  project <- renv_tests_scope()

  renv_scope_envvars(RENV_PROFILE = NULL)
  init(profile = "testing")

  # make sure we have renv installed for this test
  libpaths <- the$default_libpaths[[".libPaths()"]]
  source <- find.package("renv", lib.loc = libpaths)
  renv_imbue_self(project)

  # check that profile was written
  expect_true(file.exists("renv/profile"))

  # check that its contents equal 'testing'
  contents <- readLines("renv/profile")
  expect_equal(contents, "testing")

  # check that an R session launched here gets that profile
  renv_scope_envvars(R_PROFILE_USER = NULL)
  script <- renv_test_code({
    writeLines(Sys.getenv("RENV_PROFILE"))
  })

  args <- c("-s", "-f", shQuote(script))
  output <- renv_system_exec(R(), args, action = "reading profile")

})

test_that("a profile changes the default library / lockfile path", {
  skip_on_cran()

  renv_tests_scope()
  renv_scope_envvars(RENV_PROFILE = "testing")

  project <- getwd()
  init()

  # NOTE: renv/profile should not be written here as we've only forced
  # activation via an environment variable and not explicitly via API
  profile <- file.path(project, "renv/profile")
  expect_false(file.exists(profile))

  # however, other paths should resolve relative to the active profile
  prefix <- "renv/profiles/testing"

  expect_equal(
    renv_path_normalize(paths$lockfile(project = project)),
    renv_path_normalize(file.path(project, prefix, "renv.lock"))
  )

  expect_equal(
    renv_path_normalize(paths$library(project = project)),
    renv_path_normalize(file.path(project, prefix, "renv/library", renv_platform_prefix()))
  )

  expect_equal(
    renv_path_normalize(paths$settings(project = project)),
    renv_path_normalize(file.path(project, prefix, "renv/settings.json"))
  )

})

test_that("profile-specific dependencies can be written", {

  renv_tests_scope()

  # initialize project with 'testing' profile
  renv_scope_envvars(RENV_PROFILE = "testing")
  init()

  # have this profile depend on 'toast'
  path <- renv_paths_renv("_dependencies.R")
  ensure_parent_directory(path)
  writeLines("library(toast)", con = path)

  # validate the dependency is included
  deps <- dependencies()
  expect_true("toast" %in% deps$Package)

  # switch to other profile
  renv_scope_envvars(RENV_PROFILE = "other")

  # 'toast' is no longer required
  deps <- dependencies()
  expect_false("toast" %in% deps$Package)

})

test_that("profile-specific dependencies can be declared in DESCRIPTION", {
  renv_tests_scope()

  renv_scope_envvars(RENV_PROFILE = "testing")
  init()

  writeLines(
    "Config/renv/profiles/testing/dependencies: toast",
    con = "DESCRIPTION"
  )

  deps <- dependencies()
  expect_true("toast" %in% deps$Package)

})

test_that("profile-specific remotes are parsed", {
  project <- renv_tests_scope()

  renv_scope_envvars(RENV_PROFILE = "testing")
  init()

  desc <- heredoc('
    Type: Project
    Config/renv/profiles/testing/dependencies: bread
    Config/renv/profiles/testing/remotes: bread@0.1.0
  ')

  writeLines(desc, con = "DESCRIPTION")
  remotes <- renv_project_remotes(project)

  actual <- resolve(remotes$bread)
  expected <- list(Package = "bread", Version = "0.1.0", Source = "Repository")
  expect_equal(actual, expected)

})
