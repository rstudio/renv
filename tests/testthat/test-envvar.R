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

test_that("renv_envvars_save() is idempotent", {

  renv_scope_envvars(
    R_LIBS = NA,
    R_LIBS_SITE = NA,
    R_LIBS_USER = NA,
    RENV_DEFAULT_R_LIBS_USER = NA
  )

  renv_envvars_restore()
  before <- Sys.getenv()

  userlib <- Sys.getenv("R_LIBS_USER")
  expect_true(renv_envvars_save())
  expect_false(renv_envvars_save())
  expect_equal(userlib, Sys.getenv("RENV_DEFAULT_R_LIBS_USER"))

  during <- Sys.getenv()
  expect_false(identical(before, during))

  renv_envvars_restore()
  expect_equal(userlib, Sys.getenv("R_LIBS_USER"))

  after <- Sys.getenv()

  expect_equal(names(before), names(after))
  nms <- union(names(before), names(after))
  expect_equal(before[nms], after[nms])

})


test_that("RENV_PATHS_PREFIX is not normalized", {
  renv_scope_envvars(RENV_PATHS_PREFIX = ".")
  renv_envvars_normalize()
  expect_identical(Sys.getenv("RENV_PATHS_PREFIX"), ".")
})
