
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

test_that("the sandbox is unlocked on process exit", {
  skip_on_cran()

  renv_scope_envvars(RENV_SANDBOX_LOCKING_ENABLED = "TRUE")

  project <- tempfile("renv-project-")

  script <- renv_test_code({
    renv::init(project)
  }, list(project = project))

  output <- renv_system_exec(
    command = R(),
    args    = c("--vanilla", "-s", "-f", shQuote(script)),
    action  = "initializing project"
  )

  sandbox <- renv_paths_sandbox(project)
  expect_false(renv_sandbox_locked(sandbox))

  script <- renv_test_code({
    renv::load(project)
    locked <- renv:::renv_sandbox_locked(.Library)
    cat(locked, sep = "\n")
  }, list(project = project))

  output <- renv_system_exec(
    command = R(),
    args    = c("-s", "-f", shQuote(script)),
    action  = "checking sandbox state"
  )

  expect_equal(output, "TRUE")
  expect_false(renv_sandbox_locked(sandbox))

})

test_that("re-activate sandbox when all is said and done", {
  options(renv.config.sandbox.enabled = TRUE)
  renv_sandbox_activate()
  expect_false(.Library == renv_libpaths_system())
})

