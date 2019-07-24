
context("Sandbox")

test_that("the sandbox can be activated and deactivated", {

  renv_sandbox_deactivate()
  syslib <- .Library
  renv_sandbox_activate()
  expect_false(identical(syslib, .Library))
  renv_sandbox_deactivate()
  expect_true(identical(syslib, .Library))

})

test_that("multiple attempts to activate sandbox are handled", {

  renv_sandbox_deactivate()
  syslib <- .Library
  renv_sandbox_activate()
  renv_sandbox_activate()
  renv_sandbox_activate()
  expect_false(identical(syslib, .Library))
  renv_sandbox_deactivate()
  expect_true(identical(syslib, .Library))

})
