
test_that("the version of renv in a project can be changed (upgraded)", {

  skip_slow()

  renv_tests_scope()
  init()

  # with a version number
  upgrade(version = "0.5.0")
  version <- renv_activate_version(".")
  expect_equal(c(version), "0.5.0")

  sha <- attr(version, "sha", exact = TRUE)
  expect_equal(sha, "32f0f78d87150a8656a99223396f844e2fac7a17")

  # or with a sha
  expect_true(upgrade(version = "5049cef8a"))

  version <- renv_activate_version(".")
  expect_equal(c(version), "0.17.3-62")

  sha <- attr(version, "sha", exact = TRUE)
  expect_equal(sha, "5049cef8a94591b802f9766a0da092780f59f7e4")

  # second upgrade does nothing
  expect_false(upgrade(version = "5049cef8a"))

})

test_that("renv is taken from GitHub when only crandb knows of a release", {

  renv_tests_scope()
  renv_scope_options(renv.config.crandb.enabled = TRUE)

  # repositories which don't provide renv
  archive <- renv_tests_archive_repo()
  renv_scope_options(repos = c(ARCHIVED = archive$url))

  # crandb will always name a release of renv, but that doesn't mean the
  # repositories in use can supply it
  local_mocked_bindings(
    renv_available_packages_latest_crandb = function(package, ...) {
      list(
        Package    = package,
        Version    = "9.9.9",
        Source     = "Repository",
        Repository = "CRAN"
      )
    }
  )

  local_mocked_bindings(
    renv_upgrade_find_record_dev = function(version = NULL) "github"
  )

  expect_identical(renv_upgrade_find_record_default(), "github")

})
