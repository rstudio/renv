
test_that("system requirements are reported", {

  skip_on_cran()

  renv_scope_binding(the, "os", "linux")
  renv_scope_binding(the, "platform", list(VERSION_ID = "24.04"))

  local({
    renv_scope_binding(the, "distro", "ubuntu")
    sysdep <- renv_sysreqs_resolve("zlib")
    expect_equal(sysdep$packages, list("zlib1g-dev"))
  })

  local({
    renv_scope_binding(the, "distro", "redhat")
    sysdep <- renv_sysreqs_resolve("zlib")
    expect_equal(sysdep$packages, list("zlib-devel"))
  })

})

test_that("version constraints are respected", {

  skip_on_cran()

  renv_tests_scope()
  renv_scope_binding(the, "os", "linux")

  local({
    renv_scope_binding(the, "distro", "rockylinux")
    renv_scope_binding(the, "platform", list(VERSION_ID = "8"))
    sysdep <- renv_sysreqs_resolve("libgit2")
    expect_equal(sysdep$packages, list("libgit2_1.7-devel"))
  })

  local({
    renv_scope_binding(the, "distro", "rockylinux")
    renv_scope_binding(the, "platform", list(VERSION_ID = "9"))
    sysdep <- renv_sysreqs_resolve("libgit2")
    expect_equal(sysdep$packages, list("libgit2-devel"))
  })

})

test_that("system requirements are reported as expected", {

  skip_on_cran()

  renv_scope_binding(the, "os", "linux")
  renv_scope_binding(the, "distro", "ubuntu")
  renv_scope_binding(the, "platform", list(VERSION_ID = "24.04"))

  renv_sysreqs_check("zlib", FALSE)

})
