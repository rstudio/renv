
context("Available Packages")

test_that("renv_available_packages() errs on incorrect repository", {
  skip_on_cran()
  renv_scope_options(repos = c(CRAN = "https://www.example.com/no/such/repository"))
  expect_error(renv_available_packages(type = "source"))
})
