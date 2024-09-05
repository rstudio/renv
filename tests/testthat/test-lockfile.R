
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

  expect_equal(lock$R$Version, "4.2.1")
  expect_equal(lock$R$Repositories, list(CRAN = "https://cloud.r-project.org"))

})

test_that("we create lockfile from a manifest automatically when no lockfile found", {

  skip_on_cran()

  project_dir <- tempfile()
  dir.create(project_dir)

  manifest <- "resources/manifest.json"
  expected_lock <- renv_lockfile_from_manifest("resources/manifest.json")
  file.copy(manifest, file.path(project_dir, "manifest.json"))

  # when called with `strict = TRUE` does not create manifest
  expect_error(renv_lockfile_load(project_dir, strict = TRUE))

  # creates and reads lockfile
  obtained_lock <- renv_lockfile_load(project_dir)
  expect_identical(expected_lock, obtained_lock)
  expect_true(file.exists(file.path(project_dir, "renv.lock")))

  unlink(project_dir, recursive = TRUE)
})

test_that("the Requirements field is read as character", {

  lockfile <- renv_lockfile_read(text = '
{
  "R": {
    "Version": "2.15.2",
    "Repositories": []
  },
  "Packages": {
    "morning": {
      "Package": "morning",
      "Version": "0.1.0",
      "Requirements": [
        "coffee",
        "toast"
      ]
    }
  }
}
')

  actual <- lockfile$Packages$morning$Requirements
  expected <- c("coffee", "toast")
  expect_identical(actual, expected)

})

test_that("lockfile APIs can be used", {
  renv_tests_scope("breakfast")
  init()

  lockfile <- lockfile_read()
  expect_s3_class(lockfile, "renv_lockfile")

  # try writing some repositories
  repos <- list(CRAN = "https://cloud.r-project.org")
  lockfile <- lockfile_modify(lockfile, repos = repos)
  expect_equal(repos, lockfile$R$Repositories)

  # try updating a record
  lockfile <- lockfile_modify(lockfile, remotes = list(bread = "bread@0.1.0"))
  bread <- lockfile$Packages$bread
  expect_equal(bread$Version, "0.1.0")

  # try writing to file
  lockfile_write(lockfile)

  # check that it's the same
  expect_equal(lockfile, lockfile_read())

})
