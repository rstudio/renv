
test_that("the sandbox can be activated and deactivated", {

  renv_scope_options(renv.config.sandbox.enabled = TRUE)
  renv_scope_envvars(RENV_PATHS_SANDBOX = tempdir())

  # save current library paths
  libpaths <- .libPaths()

  # after activating the sandbox, .Library should be changed
  syslib <- renv_libpaths_system()
  renv_sandbox_activate()
  expect_false(identical(syslib, .Library))

  # after deactivating the sandbox, .Library should be restored
  renv_sandbox_deactivate()
  expect_equal(syslib, .Library)

  # the library paths should be restored as well
  expect_equal(libpaths, .libPaths())

})

test_that("multiple attempts to activate sandbox are handled", {

  renv_scope_options(renv.config.sandbox.enabled = TRUE)
  renv_scope_envvars(RENV_PATHS_SANDBOX = tempdir())

  libpaths <- .libPaths()
  syslib <- renv_libpaths_system()

  # calls to renv_sandbox_activate() should be idempotent
  renv_sandbox_activate()
  renv_sandbox_activate()
  renv_sandbox_activate()
  expect_false(identical(syslib, .Library))

  # deactivate the sandbox and assert we've restored state
  renv_sandbox_deactivate()
  expect_equal(syslib, .Library)
  expect_equal(libpaths, .libPaths())

})

test_that(".Library.site isn't used even when sandbox is disabled", {

  skip_on_os("windows")
  skip_if(empty(.Library.site))

  renv_scope_options(renv.config.sandbox.enabled = FALSE)
  renv_scope_envvars(RENV_PATHS_SANDBOX = tempdir())

  sitelib <- setdiff(.Library.site, .Library)
  renv_sandbox_activate()
  expect_false(any(sitelib %in% .libPaths()))
  renv_sandbox_deactivate()

})
