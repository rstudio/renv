
skip_if_no_packrat <- function() {

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
  cache <- renv_scope_tempfile("packrat-cache-")
  root <- renv_scope_tempfile("renv-root-")
  renv_scope_envvars(R_PACKRAT_CACHE_DIR = cache, RENV_PATHS_ROOT = root)

  # set up
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
