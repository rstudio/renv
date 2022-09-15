
context("Sandbox")

test_that("the sandbox can be activated and deactivated", {

  renv_sandbox_deactivate()
  renv_scope_options(renv.config.sandbox.enabled = TRUE)
  renv_scope_envvars(RENV_PATHS_SANDBOX = tempdir())
  libpaths <- .libPaths()
  syslib <- .Library
  renv_sandbox_activate()
  expect_false(identical(syslib, .Library))
  renv_sandbox_deactivate()
  expect_true(identical(syslib, .Library))
  expect_equal(libpaths, .libPaths())

})

test_that("multiple attempts to activate sandbox are handled", {

  renv_sandbox_deactivate()
  renv_scope_options(renv.config.sandbox.enabled = TRUE)
  renv_scope_envvars(RENV_PATHS_SANDBOX = tempdir())
  libpaths <- .libPaths()
  syslib <- .Library
  renv_sandbox_activate()
  renv_sandbox_activate()
  renv_sandbox_activate()
  expect_false(identical(syslib, .Library))
  renv_sandbox_deactivate()
  expect_true(identical(syslib, .Library))
  expect_equal(libpaths, .libPaths())

})

test_that(".Library.site isn't used even when sandbox is disabled", {

  skip_if(renv_platform_windows() || empty(.Library.site))
  renv_sandbox_deactivate()
  renv_scope_options(renv.config.sandbox.enabled = FALSE)
  renv_scope_envvars(RENV_PATHS_SANDBOX = tempdir())
  sitelib <- setdiff(.Library.site, .Library)
  renv_sandbox_activate()
  expect_false(any(sitelib %in% .libPaths()))
  renv_sandbox_deactivate()

})

test_that("re-activate sandbox when all is said and done", {
  options(renv.config.sandbox.enabled = TRUE)
  renv_sandbox_activate()
  expect_false(.Library == renv_libpaths_system())
})

