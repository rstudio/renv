
test_that("can add before or after", {
  path_string <- function(...) {
    paste(c(...), collapse = .Platform$path.sep)
  }

  renv_scope_envvars(TESTPATH = "a")
  renv_envvar_path_add("TESTPATH", "b", prepend = TRUE)
  expect_equal(Sys.getenv("TESTPATH"), path_string(c("b", "a")))

  renv_scope_envvars(TESTPATH = "a")
  renv_envvar_path_add("TESTPATH", "b", prepend = FALSE)
  expect_equal(Sys.getenv("TESTPATH"), path_string(c("a", "b")))
})

test_that("renv_envvar_path_modify doesn't duplicate paths", {
  path_string <- function(...) {
    paste(c(...), collapse = .Platform$path.sep)
  }
  renv_scope_envvars(TESTPATH = path_string("a", "b", "c"))

  renv_envvar_path_add("TESTPATH", "a")
  expect_equal(Sys.getenv("TESTPATH"), path_string("a", "b", "c"))

  renv_envvar_path_add("TESTPATH", "b")
  expect_equal(Sys.getenv("TESTPATH"), path_string("b", "a", "c"))
})

test_that("renv_envvar_path_modify works if var isn't set", {
  renv_scope_envvars(TESTPATH = NULL)

  renv_envvar_path_add("TESTPATH", "a")
  expect_equal(Sys.getenv("TESTPATH"), "a")
})

test_that("renv_envvar_exists behaves as expected", {
  renv_scope_envvars(RENV_TEST = NULL)
  expect_false(renv_envvar_exists("RENV_TEST"))
  renv_scope_envvars(RENV_TEST = 1)
  expect_true(renv_envvar_exists("RENV_TEST"))
})

