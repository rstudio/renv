context("Sandbox")

renv_scoped_sandbox <- function(envir = parent.frame()) {
  renv_scope_options(renv.config.sandbox.enabled = TRUE, envir = envir)
  renv_scope_envvars(RENV_PATHS_SANDBOX = tempdir(), envir = envir)

  old <- list(.Library.site, .Library, .libPaths())
  defer(envir = envir, {
    renv_binding_replace(".Library.site", old[[1]], envir = base)
    renv_binding_replace(".Library", old[[2]], envir = base)
    .libPaths(old[[3]])
  })
}

test_that("the sandbox can be activated and deactivated", {

  renv_scoped_sandbox()

  libpaths <- .libPaths()
  syslib <- .Library
  renv_sandbox_activate()
  expect_false(identical(syslib, .Library))
  renv_sandbox_deactivate()
  expect_true(identical(syslib, .Library))
  expect_equal(libpaths, .libPaths())

})

test_that("multiple attempts to activate sandbox are handled", {

  renv_scoped_sandbox()

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

  renv_scoped_library_restore()
  renv_scope_options(renv.config.sandbox.enabled = FALSE)
  renv_scope_envvars(RENV_PATHS_SANDBOX = tempdir())
  sitelib <- setdiff(.Library.site, .Library)
  renv_sandbox_activate()
  expect_false(any(sitelib %in% .libPaths()))
  renv_sandbox_deactivate()

})
