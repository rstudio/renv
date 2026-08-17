
test_that("system requirements are reported", {

  skip_on_cran()

  renv_scope_binding(the, "os", "linux")

  local({
    renv_scope_binding(the, "distro", "ubuntu")
    renv_scope_binding(the, "platform", list(VERSION_ID = "24.04"))
    sysdep <- renv_sysreqs_resolve("zlib")
    expect_equal(sysdep$packages, list("zlib1g-dev"))
  })

  local({
    renv_scope_binding(the, "distro", "redhat")
    renv_scope_binding(the, "platform", list(VERSION_ID = "9"))
    sysdep <- renv_sysreqs_resolve("zlib")
    expect_equal(sysdep$packages, list("zlib-devel"))
  })

})

test_that("all matching rules are reported for multi-library requirements", {

  skip_on_cran()

  renv_scope_binding(the, "os", "linux")
  renv_scope_binding(the, "distro", "ubuntu")
  renv_scope_binding(the, "platform", list(VERSION_ID = "24.04"))

  # e.g. from the 'ragg' package -- https://github.com/rstudio/renv/issues/2352
  sysreq <- "freetype2, libpng, libtiff, libjpeg, libwebp,\nlibwebpmux"
  sysdep <- renv_sysreqs_resolve(sysreq)

  expect_equal(
    sysdep$packages,
    list("libfreetype6-dev", "libjpeg-dev", "libpng-dev", "libtiff-dev", "libwebp-dev")
  )

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
  skip_if(!renv_platform_linux())
  skip_if(!nzchar(Sys.which("dpkg-query")))

  # check a package that is unlikely to be installed
  status <- system("dpkg-query -W blender 2> /dev/null")
  skip_if(status == 0L)

  sysreqs <- list("<unknown>" = "blender")
  expect_snapshot(
    . <- renv_sysreqs_check(sysreqs, FALSE),
    transform = function(x) gsub("sudo \\S+ \\S+", "sudo <install>", x)
  )

})

test_that("system requirements for alternate distributions are reported", {

  skip_on_cran()
  skip_if(!renv_platform_linux())

  packages <- c("magick", "tesseract")
  expect_snapshot(
    . <- sysreqs(packages, distro = "redhat:8"),
    transform = function(x) gsub("\\[\\d+/\\d+\\] ", "", x)
  )

})
