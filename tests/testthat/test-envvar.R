context("envvar")

test_that("renv_envvar_prepend doesn't duplicate paths", {
  path_string <- function(...) {
    paste(c(...), collapse = .Platform$path.sep)
  }

  withr::local_envvar(TESTPATH = path_string("a", "b", "c"))
  expect_equal(
    renv_envvar_prepend("TESTPATH", "a"),
    path_string("a", "b", "c")
  )
  expect_equal(
    renv_envvar_prepend("TESTPATH", "b"),
    path_string("b", "a", "c")
  )
})
