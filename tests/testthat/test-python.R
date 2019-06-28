
context("Python")

python <- "python3"

test_that("we can activate Python with a project", {

  skip_if_no_python(python)

  renv_tests_scope("breakfast")
  renv::use_python(python = python, type = "system")

  lockfile <- renv_lockfile_read("renv.lock")
  expect_true(lockfile$Python$Type == "system")

})

test_that("we can activate Python with a virtualenv in a project", {

  skip_if_no_virtualenv(python)

  renv_tests_scope("breakfast")
  renv::use_python(python = python, type = "virtualenv")

  lockfile <- renv_lockfile_read("renv.lock")
  expect_equal(lockfile$Python$Type, "virtualenv")

})

test_that("the set of installed Python packages is snapshotted", {

  skip_if_no_virtualenv(python)

  renv_tests_scope("breakfast")
  renv::use_python(type = "virtualenv")

  python <- Sys.getenv("RETICULATE_PYTHON")
  cmd <- paste(shQuote(python), "-m pip install --quiet numpy")
  system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
  renv::snapshot()

  expect_true(file.exists("requirements.txt"))
  reqs <- renv_read_properties("requirements.txt", delimiter = "==")
  expect_true("numpy" %in% names(reqs))

})
