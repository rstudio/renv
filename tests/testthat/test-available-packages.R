
context("Available Packages")

test_that("renv_available_packages() errs on incorrect repository", {
  skip_on_cran()
  renv_scope_options(repos = c(CRAN = "https://www.example.com/no/such/repository"))
  expect_error(renv_available_packages(type = "source"))
})

test_that("renv handles multiple available source packages", {
  skip_on_cran()

  renv_scope_options(repos = getOption("repos"))
  renv_tests_scope()
  repos <- tempfile("renv-test-repos-")
  renv_tests_init_repos(repos)

  dbs <- renv_available_packages(type = "source")
  cran <- dbs[["CRAN"]]
  entries <- cran[cran$Package == "breakfast", ]
  expect_true(nrow(entries) == 2)

  entry <- renv_available_packages_entry(
    package = "breakfast",
    type    = "source"
  )

  expect_true(nrow(entry) == 1)
  expect_true(entry$Package == "breakfast")
  expect_true(entry$Version == "1.0.0")

})

test_that("renv_available_packages() succeeds with unnamed repositories", {
  skip_on_cran()
  renv_tests_scope()
  renv_scope_options(repos = unname(getOption("repos")))

  entry <- renv_available_packages_entry(
    package = "breakfast",
    type    = "source",
    filter  = "1.0.0"
  )

  expect_identical(entry$Package, "breakfast")
  expect_identical(entry$Version, "1.0.0")

})

test_that("renv_available_packages_latest() respects pkgType option", {

  skip_on_cran()
  skip_if(.Platform$pkgType == "source")
  renv_tests_scope()

  renv_scope_options(pkgType = "source")
  record <- renv_available_packages_latest("breakfast")
  expect_identical(attr(record, "type"), "source")

  # NOTE: this fails because we don't populate binary repositories during tests
  renv_scope_options(pkgType = "binary")
  expect_error(renv_available_packages_latest("breakfast"))

})
