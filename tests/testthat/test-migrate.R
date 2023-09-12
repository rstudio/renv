
skip_if_no_packrat <- function() {

  # TODO: I don't understand why these errors are popping up on CI.
  #
  # Error in getSourceForPkgRecord(pkgRecord, sourceDir, availablePkgs, repos) :
  # Failed to retrieve package sources for packrat 0.9.2 from CRAN (internet connectivity issue?) [0.9.1 is current]
  skip_on_ci()

  skip_on_cran()
  skip_on_windows()
  skip_if_not_installed("packrat")

  version <- packageVersion("packrat")
  if (renv_version_length(version) != 3)
    skip("cannot test with development version of Packrat")

  packrat <- renv_available_packages_latest(package = "packrat", type = "source")
  if (version != packrat$Version)
    skip("packrat is not current")

  TRUE

}

test_that("a sample Packrat project can be migrated", {
  skip_if_no_packrat()

  # use dummy caches for this test
  renv_scope_envvars(
    R_PACKRAT_CACHE_DIR = renv_scope_tempfile("packrat-cache-"),
    RENV_PATHS_ROOT     = renv_scope_tempfile("renv-cache-")
  )

  requireNamespace("packrat")
  renv_tests_scope("breakfast")

  # initialize packrat
  quietly(
    expect_warning(
      packrat::init(enter = FALSE, options = list(use.cache = TRUE))
    )
  )

  # try to migrate
  migrate()

  # packages we expect to find
  expected <- c("bread", "breakfast", "oatmeal", "toast", "packrat")

  # check the renv cache
  cachelist <- renv_cache_list()
  expect_setequal(expected, basename(cachelist))

  # check the renv sources directory
  cransources <- renv_paths_source("cran")
  expect_setequal(expected, list.files(cransources))

  # check the lockfile
  lockfile <- renv_lockfile_read("renv.lock")
  records <- renv_lockfile_records(lockfile)
  expect_setequal(expected, names(records))

})

test_that("a Packrat project with no library can be migrated", {
  skip_if_no_packrat()

  # TODO: skip tests if non-CRAN packrat is installed

  # use dummy caches for this test
  renv_scope_envvars(
    R_PACKRAT_CACHE_DIR = renv_scope_tempfile("packrat-cache-"),
    RENV_PATHS_ROOT     = renv_scope_tempfile("renv-cache-")
  )

  renv_tests_scope("breakfast")

  # initialize packrat
  quietly(
    expect_warning(
      packrat::init(enter = FALSE, options = list(use.cache = TRUE))
    )
  )

  # remove library
  unlink("packrat/lib", recursive = TRUE)

  # try to migrate
  migrate()

  # packages we expect to find
  expected <- c("bread", "breakfast", "oatmeal", "toast", "packrat")

  # check the renv cache
  cachelist <- renv_cache_list()
  expect_setequal(expected, basename(cachelist))

  # check the lockfile
  lockfile <- renv_lockfile_read("renv.lock")
  records <- renv_lockfile_records(lockfile)
  expect_setequal(expected, names(records))

})
