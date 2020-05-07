
context("Repositories")

test_that("we can query our local repository during tests", {

  exclude <- if (renv_platform_unix()) "windowsonly" else "unixonly"
  expected <- setdiff(list.files("packages"), exclude)

  renv_tests_scope()
  ap <- renv_available_packages(type = "source")[[1]]
  expect_setequal(ap$Package, expected)

})

test_that("repository names are not lost in the lockfile", {

  url <- "https://cloud.r-project.org"
  renv_scope_options(repos = c(Example = url))
  lockfile <- renv_lockfile_init(project = getwd())
  expect_equal(lockfile$R$Repositories, list(Example = url))

})

test_that("trailing slashes are removed from repositories on load", {
  renv_scope_options(repos = NULL)
  renv_load_r_repos(list(CRAN = "https://cloud.r-project.org/"))
  expect_equal(getOption("repos"), c(CRAN = "https://cloud.r-project.org"))
})
