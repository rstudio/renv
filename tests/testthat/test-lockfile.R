
context("Lockfile")

test_that("lockfiles can be diffed", {

  lhs <- list(A = 1, B = 2, C = "a", D = list(E = 1, F = 2))
  rhs <- list(A = 1, B = 3, C = "b", D = list(E = 1, F = 3))

  diff <- renv_lockfile_diff(lhs, rhs)
  expected <- list(
    B = list(before = 2, after = 3),
    C = list(before = "a", after = "b"),
    D = list(
      F = list(before = 2, after = 3)
    )
  )

  expect_identical(diff, expected)

})

test_that("we can serialize lockfiles using unnamed repositories", {

  # no repositories set
  local({
    renv_scope_options(repos = list())
    actual <- renv_lockfile_init(project = NULL)
    json <- renv_lockfile_write(actual, file = NULL)
    expected <- renv_lockfile_read(text = json)
    expect_equal(actual, expected)
  })

  # unnamed repositories set
  local({
    renv_scope_options(repos = c("alpha", "beta"))
    actual <- renv_lockfile_init(project = NULL)
    json <- renv_lockfile_write(actual, file = NULL)
    expected <- renv_lockfile_read(text = json)
    expect_equal(actual, expected)
  })

})

test_that("we can create lockfiles from manifests", {

  skip_on_cran()
  lock <- renv_lockfile_from_manifest("resources/manifest.json")

  expect_equal(lock$R$Version, numeric_version("4.2.1"))
  expect_equal(lock$R$Repositories, list(CRAN = "https://cloud.r-project.org"))

})
