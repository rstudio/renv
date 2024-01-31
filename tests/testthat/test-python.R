
renv_test_scope_python <- function(scope = parent.frame()) {
  renv_scope_envvars(
    PATH = Sys.getenv("PATH"),
    RENV_PYTHON = NULL,
    RETICULATE_PYTHON = NULL,
    RETICULATE_PYTHON_ENV = NULL,
    scope = scope
  )
}

test_that("we can activate Python with a project", {

  skip_on_os("windows")
  skip_on_cran()
  skip_if_no_python()
  renv_test_scope_python()

  renv_tests_scope("breakfast")
  use_python(python = sys_python(), type = "system")

  lockfile <- renv_lockfile_read("renv.lock")
  expect_true(!is.null(lockfile$Python))

})

test_that("we can activate Python with a virtualenv in a project", {

  skip_slow()
  skip_on_os("windows")
  skip_if_no_virtualenv()
  renv_test_scope_python()

  renv_tests_scope("breakfast")
  use_python(python = sys_python(), type = "virtualenv")

  lockfile <- renv_lockfile_read("renv.lock")
  expect_equal(lockfile$Python$Type, "virtualenv")

})

test_that("renv uses local virtual environment for names beginning with '.'", {

  skip_on_os("windows")
  skip_on_cran()
  skip_if_no_virtualenv()
  renv_test_scope_python()

  renv_tests_scope("breakfast")
  use_python(python = sys_python(), name = ".venv")

  expect_true(renv_file_exists(".venv"))

  lockfile <- renv_lockfile_read("renv.lock")
  expect_equal(lockfile$Python$Type, "virtualenv")
  expect_equal(lockfile$Python$Name, ".venv")

})

test_that("renv can bind to virtualenvs in WORKON_HOME", {

  skip_on_os("windows")
  skip_on_cran()
  skip_if_no_virtualenv()
  renv_test_scope_python()

  # work with temporary virtualenv home
  renv_scope_envvars(WORKON_HOME = tempdir())
  home <- renv_python_virtualenv_home()
  expect_true(renv_path_same(home, tempdir()))

  # construct environment name, path
  name <- "renv-test-environment"
  path <- file.path(home, name)

  # clean up when we're done
  defer(unlink(path, recursive = TRUE))

  # create a test project
  renv_tests_scope("breakfast")
  use_python(python = sys_python(), name = "renv-test-environment")

  expect_true(renv_file_exists(path))

  lockfile <- renv_lockfile_read("renv.lock")
  expect_equal(lockfile$Python$Type, "virtualenv")
  expect_equal(lockfile$Python$Name, "renv-test-environment")

})

test_that("installed Python packages are snapshotted / restored [virtualenv]", {

  skip_slow()
  skip_on_os("windows")
  skip_if_no_virtualenv()
  renv_test_scope_python()

  renv_tests_scope("breakfast")

  python <- sys_python()
  # initialize python
  python <- use_python(
    python,
    name = renv_scope_tempfile("python-virtualenv-"),
    type = "virtualenv"
  )

  # install python-dotenv
  expect_false(renv_python_module_available(python, "dotenv"))
  pip_install("python-dotenv", python = python)
  expect_true(renv_python_module_available(python, "dotenv"))

  # snapshot changes
  snapshot()

  # check requirements.txt for install
  expect_true(file.exists("requirements.txt"))
  reqs <- renv_properties_read("requirements.txt", delimiter = "==")
  expect_true("python-dotenv" %in% names(reqs))

  # uninstall python-dotenv
  expect_true(renv_python_module_available(python, "dotenv"))
  pip_uninstall("python-dotenv", python = python)
  expect_false(renv_python_module_available(python, "dotenv"))

  # try to restore
  restore()

  # check that we can load python-dotenv now
  expect_true(renv_python_module_available(python, "dotenv"))

})


test_that("installed Python packages are snapshotted / restored [conda]", {

  skip_if_local()
  skip_on_os("windows")
  skip_on_cran()
  skip_if_no_miniconda("3.6")
  renv_test_scope_python()

  renv_tests_scope("breakfast")

  # initialize python
  quietly(use_python(type = "conda"))
  python <- Sys.getenv("RETICULATE_PYTHON")

  # install numpy
  cmd <- paste(shQuote(python), "-m pip install --quiet numpy")
  system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)

  # snapshot changes
  snapshot()

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
  restore()

  # check that we can load numpy now
  cmd <- paste(shQuote(python), "-c 'import numpy'")
  status <- system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
  expect_true(status == 0L)

})

test_that("python environment name is preserved after snapshot", {

  skip_if_local()
  skip_on_os("windows")
  skip_on_cran()
  skip_if_no_miniconda("3.6")
  renv_test_scope_python()

  # create and use local python environment
  renv_tests_scope()
  init()

  system2(python, c("-m", "venv", "virtualenv"))
  use_python(name = "./virtualenv")

  # check that the lockfile has been updated
  lockfile <- renv_lockfile_read("renv.lock")
  expect_equal(lockfile$Python$Name, "./virtualenv")

  # try to snapshot
  snapshot()

  # check that the virtual environment name was preserved
  lockfile <- renv_lockfile_read("renv.lock")
  expect_equal(lockfile$Python$Name, "./virtualenv")

})

test_that("renv_python_discover() respects PATH ordering", {

  skip_on_cran()
  skip_on_windows()
  renv_tests_scope()
  renv_test_scope_python()

  # create a bunch of python directories
  wd <- renv_path_normalize(getwd())
  pythons <- file.path(wd, c("1", "2", "3"), "python")
  for (python in pythons) {
    ensure_parent_directory(python)
    file.create(python)
    Sys.chmod(python, "0755")
  }

  # update the path
  path <- paste(dirname(pythons), collapse = .Platform$path.sep)
  discovered <- local({
    renv_scope_envvars(PATH = path)
    renv_python_discover()
  })

  # check that we looked at them in the right order
  expect_equal(tail(discovered, n = 3L), pythons)

})

test_that("renv uses symlinks for system python", {

  skip_on_cran()
  skip_on_windows()
  renv_tests_scope()
  renv_test_scope_python()
  skip_if(!file.exists("/usr/bin/python3"))

  use_python("/usr/bin/python3", type = "system")
  expect_false(Sys.which("python3") == "/usr/bin/python3")

})
