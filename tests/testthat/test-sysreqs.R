
test_that("system requirements are reported", {

  skip_on_cran()

  renv_tests_scope()
  renv_scope_binding(the, "os", "linux")

  local({
    renv_scope_binding(the, "distro", "ubuntu")
    syspkg <- renv_sysreqs_resolve("zlib")
    expect_equal(syspkg, "zlib1g-dev")
  })

  local({
    renv_scope_binding(the, "distro", "redhat")
    syspkg <- renv_sysreqs_resolve("zlib")
    expect_equal(syspkg, "zlib-devel")
  })

})
