
test_that("system requirements are reported", {

  skip_on_cran()

  renv_tests_scope()
  renv_scope_binding(the, "os", "linux")
  renv_scope_binding(the, "platform", list(VERSION_ID = "24.04"))

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

test_that("version constraints are respected", {

  skip_on_cran()

  renv_tests_scope()
  renv_scope_binding(the, "os", "linux")

  local({
    renv_scope_binding(the, "distro", "rockylinux")
    renv_scope_binding(the, "platform", list(VERSION_ID = "8"))
    syspkg <- renv_sysreqs_resolve("libgit2")
    expect_equal(syspkg, "libgit2_1.7-devel")
  })

  local({
    renv_scope_binding(the, "distro", "rockylinux")
    renv_scope_binding(the, "platform", list(VERSION_ID = "9"))
    syspkg <- renv_sysreqs_resolve("libgit2")
    expect_equal(syspkg, "libgit2-devel")
  })



})
