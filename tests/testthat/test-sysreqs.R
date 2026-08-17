
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

test_that("sysreqs() resolves packages from the project lockfile", {

  skip_on_cran()

  project <- renv_tests_scope()

  lockfile <- list(
    R = list(Version = "4.5.0", Repositories = list(CRAN = "https://cloud.r-project.org")),
    Packages = list(
      morning = list(
        Package = "morning",
        Version = "1.0.0",
        Source = "Repository",
        Repository = "CRAN",
        Title = "A Test Package",
        SystemRequirements = "libcurl (>= 7.62)"
      ),
      evening = list(
        Package = "evening",
        Version = "1.0.0",
        Source = "Repository",
        Repository = "CRAN",
        Title = "A Test Package",
        SystemRequirements = "freetype2, libpng"
      ),
      night = list(
        Package = "night",
        Version = "1.0.0",
        Source = "Repository",
        Repository = "CRAN",
        Title = "A Test Package"
      )
    )
  )

  renv_json_write(lockfile, file = "renv.lock")

  # all lockfile packages are used by default
  sysdeps <- sysreqs(project = project, distro = "ubuntu:24.04", report = FALSE)
  expect_setequal(names(sysdeps), c("morning", "evening", "night"))
  expect_equal(sysdeps$morning$packages, list("libcurl4-openssl-dev"))
  expect_equal(sysdeps$evening$packages, list("libfreetype6-dev", "libpng-dev"))
  expect_null(sysdeps$night)

  # explicitly-requested packages are resolved from the lockfile as well
  sysdeps <- sysreqs(packages = "evening", project = project, distro = "ubuntu:24.04", report = FALSE)
  expect_equal(names(sysdeps), "evening")
  expect_equal(sysdeps$evening$packages, list("libfreetype6-dev", "libpng-dev"))

})

test_that("legacy lockfile records are not treated as authoritative", {

  v1 <- list(
    Package = "curl",
    Version = "7.0.0",
    Source = "Repository",
    Repository = "CRAN",
    Requirements = list("R"),
    Hash = "0123456789abcdef"
  )
  expect_false(renv_sysreqs_record_authoritative(v1))

  v2 <- c(v1, list(Title = "A Modern and Flexible Web Client for R"))
  expect_true(renv_sysreqs_record_authoritative(v2))

  v1sys <- c(v1, list(SystemRequirements = "libcurl"))
  expect_true(renv_sysreqs_record_authoritative(v1sys))

})

test_that("sysreqs lookup falls back across sources", {

  # the library source resolves installed packages
  record <- renv_sysreqs_lookup("utils", sources = "library", lockfile = NULL)
  expect_equal(record$Package, "utils")

  # unresolvable packages return NULL
  expect_null(renv_sysreqs_lookup("no.such.package", sources = "library", lockfile = NULL))

})

test_that("lockfile version hints constrain library lookups", {

  lockfile <- list(
    utils = list(
      Package = "utils",
      Version = "0.1.0",
      Source = "Repository",
      Repository = "CRAN",
      Hash = "0123456789abcdef"
    )
  )

  # an installed copy with a mismatched version is only used as a last resort
  record <- renv_sysreqs_lookup("utils", sources = c("lockfile", "library"), lockfile = lockfile)
  expect_equal(record$Package, "utils")
  expect_false(identical(record$Version, "0.1.0"))

  # an installed copy with a matching version is used directly
  lockfile$utils$Version <- as.character(packageVersion("utils"))
  record <- renv_sysreqs_lookup("utils", sources = c("lockfile", "library"), lockfile = lockfile)
  expect_equal(record$Version, lockfile$utils$Version)

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
