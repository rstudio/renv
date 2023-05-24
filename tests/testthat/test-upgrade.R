
test_that("the version of renv in a project can be changed (upgraded)", {

  skip_slow()

  renv_tests_scope()

  init()
  load()

  upgrade(version = "0.5.0")

  project <- getwd()
  expect_equal(renv_activate_version(project), "32f0f78d87150a8656a99223396f844e2fac7a17")

})
