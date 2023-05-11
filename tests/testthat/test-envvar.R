context("envvar")

test_that("renv_envvar_prepend doesn't duplicate paths", {
  path_string <- function(...) {
    paste(c(...), collapse = .Platform$path.sep)
  }
  withr::local_envvar(TESTPATH = path_string("a", "b", "c"))

  renv_envvar_prepend("TESTPATH", "a")
  expect_equal(Sys.getenv("TESTPATH"), path_string("a", "b", "c"))

  renv_envvar_prepend("TESTPATH", "b")
  expect_equal(Sys.getenv("TESTPATH"), path_string("b", "a", "c"))
})

test_that("renv_envvar_prepend works if var isn't set", {

  renv_envvar_prepend("TESTPATH", "a")
  defer(Sys.unsetenv("TESTPATH"))

  expect_equal(Sys.getenv("TESTPATH"), "a")
})
