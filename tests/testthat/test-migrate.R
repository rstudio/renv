
context("Migrate")

test_that("a sample Packrat project can be migrated", {
  skip_on_cran()
  skip_if_not_installed("packrat")

  renv_tests_scope("breakfast")

  # try setting up a dummy packrat cache
  cache <- tempfile("packrat-cache-")
  ensure_directory(cache)
  Sys.setenv(R_PACKRAT_CACHE_DIR = cache)

  # initialize packrat
  sink(file = nullfile())
  suppressWarnings(
    packrat::init(enter = FALSE, options = list(use.cache = TRUE))
  )
  sink(NULL)

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
