
test_that("remove() removes packages from the project library", {

  renv_tests_scope("bread")
  init()

  expect_true(renv_package_installed("bread"))
  remove("bread")
  expect_false(renv_package_installed("bread"))

})

test_that("remove() prompts before removing from a non-project library", {

  renv_tests_scope("bread")
  init()

  library <- renv_scope_tempfile("renv-library-")
  ensure_directory(library)
  install("bread", library = library)
  expect_true(renv_package_installed("bread", lib.loc = library))

  # declining the prompt leaves the package installed
  local_mocked_bindings(proceed = function(...) FALSE)
  expect_error(remove("bread", library = library, prompt = TRUE))
  expect_true(renv_package_installed("bread", lib.loc = library))

  # accepting the prompt removes the package
  local_mocked_bindings(proceed = function(...) TRUE)
  remove("bread", library = library, prompt = TRUE)
  expect_false(renv_package_installed("bread", lib.loc = library))

})

test_that("remove() does not prompt when removing from the project library", {

  renv_tests_scope("bread")
  init()

  local_mocked_bindings(proceed = function(...) stop("prompt should not be shown"))
  remove("bread", prompt = TRUE)
  expect_false(renv_package_installed("bread"))

})
