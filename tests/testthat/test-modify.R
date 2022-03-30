
context("Modify")

test_that("lockfiles can be modified non-interactively", {

  skip_on_cran()
  renv_tests_scope()
  init()

  modify(changes = list(R = list(Version = "1.0.0")))
  lockfile <- renv_lockfile_load(project = getwd())
  expect_equal(lockfile$R$Version, "1.0.0")

})

test_that("modifications that 'break' the lockfile are not persisted", {

  skip_on_cran()
  project <- renv_tests_scope()
  init()

  old <- renv_lockfile_load(project)
  expect_error(modify(changes = list(Packages = 42)))
  new <- renv_lockfile_load(project)
  expect_equal(old, new)

})
