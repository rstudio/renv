
test_that("bulletin() creates bulleted list with optional postamble", {
  expect_snapshot({
    bulletin("preamble", letters[1:3])
    bulletin("preamble", letters[1:3], postamble = "after")
  })
})

test_that("bulletin() doesn't show pre/post amble if no values", {
  expect_silent(bulletin("before", character(), "after"))
})

test_that("options(renv.pretty.print.emitter) is respected", {

  skip_on_cran()
  project <- renv_tests_scope("bread")
  init()

  cls <- "renv.pretty.print.emitter"
  emitter <- function(text) renv_condition_signal(cls)
  renv_scope_options(renv.pretty.print.emitter = emitter)
  renv_scope_options(renv.verbose = TRUE)

  # regular pretty printer
  expect_condition(bulletin("preamble", 1), class = cls)

  # record printer
  lockfile <- renv_lockfile_create(project = getwd())
  records <- renv_lockfile_records(lockfile)
  expect_condition(renv_pretty_print_records(NULL, records), class = cls)

  # diff printer
  expect_condition(renv_pretty_print_records_pair(NULL, records, records))

})
