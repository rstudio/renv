
context("Python")

python <-
  Sys.which("python3") %""%
  Sys.which("python")  %""%
  skip("python is not available")

test_that("we can activate Python with a project", {

  skip_on_os("windows")
  skip_on_cran()
  skip_if_no_python(python)

  renv_tests_scope("breakfast")
  renv::use_python(python = python, type = "system")

  lockfile <- renv_lockfile_read("renv.lock")
  expect_true(lockfile$Python$Type == "system")

})

test_that("we can activate Python with a virtualenv in a project", {

  skip_on_os("windows")
  skip_on_cran()
  skip_if_no_virtualenv(python)

  renv_tests_scope("breakfast")
  renv::use_python(python = python, type = "virtualenv")

  lockfile <- renv_lockfile_read("renv.lock")
  expect_equal(lockfile$Python$Type, "virtualenv")

})

test_that("renv uses local virtual environment for names beginning with '.'", {

  skip_on_os("windows")
  skip_on_cran()
  skip_if_no_virtualenv(python)

  renv_tests_scope("breakfast")
  renv::use_python(python = python, name = ".venv")

  expect_true(renv_file_exists(".venv"))

  lockfile <- renv_lockfile_read("renv.lock")
  expect_equal(lockfile$Python$Type, "virtualenv")
  expect_equal(lockfile$Python$Name, ".venv")

})

test_that("renv can bind to virtualenvs in WORKON_HOME", {

  skip_on_os("windows")
  skip_on_cran()
  skip_if_no_virtualenv(python)

  # work with temporary virtualenv home
  renv_scope_envvars(WORKON_HOME = tempdir())
  home <- renv_python_virtualenv_home()
  expect_true(renv_path_same(home, tempdir()))

  # construct environment name, path
  name <- "renv-test-environment"
  path <- file.path(home, name)

  # clean up when we're done
  on.exit(unlink(path, recursive = TRUE), add = TRUE)

  # create a test project
  renv_tests_scope("breakfast")
  renv::use_python(python = python, name = "renv-test-environment")

  expect_true(renv_file_exists(path))

  lockfile <- renv_lockfile_read("renv.lock")
  expect_equal(lockfile$Python$Type, "virtualenv")
  expect_equal(lockfile$Python$Name, "renv-test-environment")

})

test_that("installed Python packages are snapshotted / restored [virtualenv]", {

  skip_on_os("windows")
  skip_on_cran()
  skip_if_no_virtualenv(python)

  Sys.unsetenv("RENV_PYTHON")
  Sys.unsetenv("RETICULATE_PYTHON")
  Sys.unsetenv("RETICULATE_PYTHON_ENV")

  renv_tests_scope("breakfast")

  # initialize python
  python <- renv::use_python(python, type = "virtualenv")

  # install python-dotenv
  expect_false(renv_python_module_available(python, "dotenv"))
  pip_install("python-dotenv", python = python)
  expect_true(renv_python_module_available(python, "dotenv"))

  # snapshot changes
  local({
    renv_scope_sink()
    renv::snapshot()
  })

  # check requirements.txt for install
  expect_true(file.exists("requirements.txt"))
  reqs <- renv_properties_read("requirements.txt", delimiter = "==")
  expect_true("python-dotenv" %in% names(reqs))

  # uninstall python-dotenv
  expect_true(renv_python_module_available(python, "dotenv"))
  pip_uninstall("python-dotenv", python = python)
  expect_false(renv_python_module_available(python, "dotenv"))

  # try to restore
  local({
    renv_scope_sink()
    renv::restore()
  })

  # check that we can load python-dotenv now
  expect_true(renv_python_module_available(python, "dotenv"))

})


test_that("installed Python packages are snapshotted / restored [conda]", {

  skip_if_local()
  skip_on_os("windows")
  skip_on_cran()
  skip_if_no_miniconda(python)
  skip_if(renv_version_lt(renv_python_version(python), "3.6"))

  Sys.unsetenv("RENV_PYTHON")
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
