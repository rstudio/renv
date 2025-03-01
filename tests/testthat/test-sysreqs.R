
test_that("system requirements are reported", {

  skip_on_cran()
  skip_if_not(renv_platform_linux())

  renv_tests_scope()

  record <- list(
    Package = "httpuv",
    SystemRequirements = "zlib"
  )

  rules <- renv_sysreqs_rules()
  matches <- renv_sysreqs_match("zlib", rules)
  expect_true(!is.null(matches$zlib.json))

  local({
    renv_scope_binding(the, "distribution", "ubuntu")
    syspkg <- renv_sysreqs_get_impl("zlib")
    expect_equal(syspkg, "zlib1g-dev")
  })

  local({
    renv_scope_binding(the, "distribution", "redhat")
    syspkg <- renv_sysreqs_get_impl("zlib")
    expect_equal(syspkg, "zlib-devel")
  })

})
