
context("Python")

python <-
  Sys.which("python3") %""%
  Sys.which("python")  %""%
  skip("python is not available")

test_that("we can activate Python with a project", {

  skip_on_appveyor()
  skip_on_cran()
  skip_if_no_python(python)

  renv_tests_scope("breakfast")
  renv::use_python(python = python, type = "system")

  lockfile <- renv_lockfile_read("renv.lock")
  expect_true(lockfile$Python$Type == "system")

})

test_that("we can activate Python with a virtualenv in a project", {

  skip_on_appveyor()
  skip_on_cran()
  skip_if_no_virtualenv(python)

  renv_tests_scope("breakfast")
  renv::use_python(python = python, type = "virtualenv")

  lockfile <- renv_lockfile_read("renv.lock")
  expect_equal(lockfile$Python$Type, "virtualenv")

})

test_that("installed Python packages are snapshotted / restored [virtualenv]", {

  skip_if_local()
  skip_on_travis()
  skip_on_appveyor()
  skip_on_cran()
  skip_if_no_virtualenv(python)

  Sys.unsetenv("RETICULATE_PYTHON")
  Sys.unsetenv("RETICULATE_PYTHON_ENV")

  renv_tests_scope("breakfast")

  # initialize python
  renv::use_python(type = "virtualenv")
  python <- Sys.getenv("RETICULATE_PYTHON")

  # install numpy
  cmd <- paste(shQuote(python), "-m pip install --quiet numpy")
  system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)

  # snapshot changes
  renv::snapshot()

  # check requirements.txt for install
  expect_true(file.exists("requirements.txt"))
  reqs <- renv_properties_read("requirements.txt", delimiter = "==")
  expect_true("numpy" %in% names(reqs))

  # uninstall numpy
  cmd <- paste(shQuote(python), "-m pip uninstall --quiet --yes numpy")
  system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)

  # can no longer load numpy
  cmd <- paste(shQuote(python), "-c 'import numpy'")
  status <- system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
  expect_false(status == 0L)

  # try to restore
  renv::restore()

  # check that we can load numpy now
  cmd <- paste(shQuote(python), "-c 'import numpy'")
  status <- system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
  expect_true(status == 0L)

})


test_that("installed Python packages are snapshotted / restored [conda]", {

  skip_if_local()
  skip_on_travis()
  skip_on_appveyor()
  skip_on_cran()
  skip_if_no_miniconda(python)

  Sys.unsetenv("RETICULATE_PYTHON")
  Sys.unsetenv("RETICULATE_PYTHON_ENV")

  renv_tests_scope("breakfast")

  # initialize python
  quietly(renv::use_python(type = "conda"))
  python <- Sys.getenv("RETICULATE_PYTHON")

  # install numpy
  cmd <- paste(shQuote(python), "-m pip install --quiet numpy")
  system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)

  # snapshot changes
  renv::snapshot()

  # check requirements.txt for install
  expect_true(file.exists("environment.yml"))

  # uninstall numpy
  cmd <- paste(shQuote(python), "-m pip uninstall --quiet --yes numpy")
  system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)

  # can no longer load numpy
  cmd <- paste(shQuote(python), "-c 'import numpy'")
  status <- system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
  expect_false(status == 0L)

  # try to restore
  renv::restore()

  # check that we can load numpy now
  cmd <- paste(shQuote(python), "-c 'import numpy'")
  status <- system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
  expect_true(status == 0L)

})

# deactivate reticulate integration
Sys.unsetenv("RENV_PYTHON")
Sys.unsetenv("RETICULATE_PYTHON")
Sys.unsetenv("RETICULATE_PYTHON_ENV")
