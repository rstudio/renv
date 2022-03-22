
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

test_that("renv emits an error if repos option is malformed", {
  renv_tests_scope()

  # disallow malformed repos
  expect_error(snapshot(repos = 1))

  # disallow unnamed repositories
  expect_error(snapshot(repos = c("a", "b")))

  # allow zero-length repos
  lockfile <- snapshot(repos = NULL, lockfile = NULL)
  expect_true(empty(lockfile$R$Repositories))

  # explicitly allow single unnamed repository
  lockfile <- snapshot(repos = "https://cloud.r-project.org", lockfile = NULL)
  expect_equal(lockfile$R$Repositories, list(CRAN = "https://cloud.r-project.org"))

  # allow custom named repositories
  repos <- list(
    CRAN = "https://cran.rstudio.com",
    RSPM = "https://packagemanager.rstudio.com/all/latest"
  )

  lockfile <- snapshot(repos = repos, lockfile = NULL)
  expect_equal(lockfile$R$Repositories, repos)

})

test_that("repository names are always set", {

  skip_on_cran()
  renv_tests_scope()

  repos <- c(CRAN = "1", "2", "3")

  actual <- renv_repos_normalize(repos)
  expected <- c(CRAN = "1", V1 = "2", V2 = "3")
  expect_equal(actual, expected)

})

