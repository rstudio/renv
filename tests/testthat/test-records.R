
context("Records")

test_that("renv_records_select() handles missing packages gracefully", {

  # simulate what happens during printing of records during install
  lhs <- list()
  rhs <- list(skeleton = list(Package = "skeleton"))

  actions <- c(skeleton = "install")
  action <- "install"

  expect_identical(renv_records_select(lhs, actions, action), lhs)
  expect_identical(renv_records_select(rhs, actions, action), rhs)

})

test_that("we can format records in various ways", {

  old <- list(
    Package    = "skeleton",
    Version    = "1.0.0",
    Source     = "Repository",
    Repository = "CRAN"
  )

  new <- list(
    Package        = "skeleton",
    Version        = "1.0.0",
    Source         = "github",
    RemoteUsername = "kevinushey",
    RemoteRepo     = "skeleton"
  )

  expect_equal(renv_record_format_short(old), "1.0.0")
  expect_equal(renv_record_format_short(new), "kevinushey/skeleton")

  expect_equal(
    renv_record_format_pair(old, new),
    "[1.0.0 -> kevinushey/skeleton]"
  )

})
