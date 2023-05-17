test_that("renv_envvars_save() is idempotent", {

  envs <- rep(list(NULL), length(renv_envvars_list()))
  names(envs) <- renv_envvars_list()
  renv_scope_envvars(list = envs)
  renv_scope_envvars(RENV_DEFAULT_R_LIBS_USER = "")

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
