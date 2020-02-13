
context("Settings")

test_that("renv.settings can be used to provide defaults", {

  renv_tests_scope()
  expect_equal(settings$snapshot.type(), "implicit")

  # project is not yet initialized, so defaults can be used
  local({
    renv_scope_options(renv.settings.snapshot.type = "all")
    expect_equal(settings$snapshot.type(), "all")
  })

  local({
    renv_scope_options(renv.settings = list(snapshot.type = "all"))
    expect_equal(settings$snapshot.type(), "all")
  })

  renv::init()

  # project is now initialized; defaults are ignored
  expect_equal(settings$snapshot.type(), "implicit")
  local({
    renv_scope_options(renv.settings = list(snapshot.type = "all"))
    expect_equal(settings$snapshot.type(), "implicit")
  })

})

test_that("non-persistent settings exist in R session; not in file", {

  renv_tests_scope()
  expect_equal(settings$snapshot.type(), "implicit")

  project <- getwd()
  path <- "renv/settings.dcf"
  before <- renv_settings_read_impl(path)
  settings$snapshot.type("all", persist = FALSE)
  after <- renv_settings_read_impl(path)

  expect_equal(before, after)
  expect_equal(settings$snapshot.type(), "all")

  settings$ignored.packages("dplyr", persist = TRUE)

  settings <- renv_settings_get(project)
  persisted <- renv_settings_read_impl(path)
  expect_mapequal(settings, persisted)

})
