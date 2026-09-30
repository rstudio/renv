test_that("install() reports a rollback when an older package remains installed", {

  renv_tests_scope()
  install("breakfast")

  local_mocked_bindings(
    renv_graph_install = function(descriptions) {
      structure(
        list(),
        rolledback = "breakfast",
        failed = "breakfast"
      )
    }
  )

  error <- expect_error(install("breakfast@0.0.1", transactional = TRUE))
  expect_match(conditionMessage(error), "breakfast")
  expect_equal(renv_package_version("breakfast"), "1.0.0")

})
