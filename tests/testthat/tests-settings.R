
context("Settings")

test_that("renv.settings can be used to provide defaults", {

  renv_tests_scope()
  expect_equal(settings$snapshot.type(), "packrat")

  # project is not yet initialized, so defaults can be used
  local({
    renv_scope_options(renv.settings.snapshot.type = "simple")
    expect_equal(settings$snapshot.type(), "simple")
  })

  local({
    renv_scope_options(renv.settings = list(snapshot.type = "simple"))
    expect_equal(settings$snapshot.type(), "simple")
  })

  renv::init()

  # project is now initialized; defaults are ignored
  expect_equal(settings$snapshot.type(), "packrat")
  local({
    renv_scope_options(renv.settings = list(snapshot.type = "simple"))
    expect_equal(settings$snapshot.type(), "packrat")
  })

})
