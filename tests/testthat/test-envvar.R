context("envvar")

test_that("can add before or after", {
  path_string <- function(...) {
    paste(c(...), collapse = .Platform$path.sep)
  }

  withr::local_envvar(TESTPATH = "a")
  renv_envvar_path_add("TESTPATH", "b", prepend = TRUE)
  expect_equal(Sys.getenv("TESTPATH"), path_string(c("b", "a")))

  withr::local_envvar(TESTPATH = "a")
  renv_envvar_path_add("TESTPATH", "b", prepend = FALSE)
  expect_equal(Sys.getenv("TESTPATH"), path_string(c("a", "b")))
})

test_that("renv_envvar_path_modify doesn't duplicate paths", {
  path_string <- function(...) {
    paste(c(...), collapse = .Platform$path.sep)
  }
  withr::local_envvar(TESTPATH = path_string("a", "b", "c"))

  renv_envvar_path_add("TESTPATH", "a")
  expect_equal(Sys.getenv("TESTPATH"), path_string("a", "b", "c"))

  renv_envvar_path_add("TESTPATH", "b")
  expect_equal(Sys.getenv("TESTPATH"), path_string("b", "a", "c"))
})

test_that("renv_envvar_path_modify works if var isn't set", {
  withr::local_envvar(TESTPATH = NA)

  renv_envvar_path_add("TESTPATH", "a")
  expect_equal(Sys.getenv("TESTPATH"), "a")
})

