
context("Repositories")

test_that("we can query our local repository during tests", {
  expected <- list.files("packages")
  renv_tests_scope()
  ap <- renv_available_packages(type = "source")[[1]]
  expect_setequal(ap$Package, expected)
})

test_that("repository names are not lost in the lockfile", {

  url <- "https://cran.rstudio.com"
  renv_scope_options(repos = c(Example = url))
  lockfile <- renv_lockfile_init(project = getwd())
  expect_equal(lockfile$R$Repositories, list(Example = url))

})
