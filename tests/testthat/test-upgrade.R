
test_that("the version of renv in a project can be changed (upgraded)", {

  skip_slow()

  renv_tests_scope()
  init()

  # with a version number
  upgrade(version = "0.5.0")
  expect_equal(
    renv_activate_version("."),
    structure("0.5.0", sha = "32f0f78d87150a8656a99223396f844e2fac7a17")
  )

  # or with a sha
  expect_true(upgrade(version = "5049cef8a"))
  expect_equal(
    renv_activate_version("."),
    structure("0.17.3-62", sha = "5049cef8a94591b802f9766a0da092780f59f7e4")
  )

  # second upgrade does nothing
  expect_false(upgrade(version = "5049cef8a"))

})
