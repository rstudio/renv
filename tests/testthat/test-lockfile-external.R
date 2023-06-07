test_that("snapshot captures pandoc and quarto versions", {
  local_mocked_bindings(
    quarto_version = function() "1.1.1",
    pandoc_version = function() "2.2.2"
  )

  renv_tests_scope()
  snapshot()

  lockfile <- renv_lockfile_read("renv.lock")
  expect_equal(lockfile$External, list(quarto = "1.1.1", pandoc = "2.2.2"))
})
