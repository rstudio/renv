
context("Migrate")

skip_if_no_packrat <- function() {
  skip_on_cran()
  skip_if_not_installed("packrat")

  version <- unclass(packageVersion("packrat"))
  if (length(version[[1]]) > 3)
    skip("cannot test with development version of Packrat")

  TRUE

}

test_that("a sample Packrat project can be migrated", {
  skip_if_no_packrat()

  # use dummy caches for this test
  renv_scope_envvars(
    R_PACKRAT_CACHE_DIR = tempfile("packrat-cache-"),
    RENV_PATHS_ROOT     = tempfile("renv-cache-")
  )

  renv_tests_scope("breakfast")

  # initialize packrat
  quietly(
    expect_warning(
      packrat::init(enter = FALSE, options = list(use.cache = TRUE))
    )
  )

  # try to migrate
  renv::migrate()

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
  records <- renv_records(lockfile)
  expect_setequal(expected, names(records))

})

test_that("a Packrat project with no library can be migrated", {
  skip_if_no_packrat()

  # TODO: skip tests if non-CRAN packrat is installed

  # use dummy caches for this test
  renv_scope_envvars(
    R_PACKRAT_CACHE_DIR = tempfile("packrat-cache-"),
    RENV_PATHS_ROOT     = tempfile("renv-cache-")
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
  renv::migrate()

  # packages we expect to find
  expected <- c("bread", "breakfast", "oatmeal", "toast", "packrat")

  # check the renv cache
  cachelist <- renv_cache_list()
  expect_setequal(expected, basename(cachelist))

  # check the lockfile
  lockfile <- renv_lockfile_read("renv.lock")
  records <- renv_records(lockfile)
  expect_setequal(expected, names(records))

})
