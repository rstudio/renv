
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

  # a package used in the project, but not yet recorded in the lockfile
  writeLines("library(toast)", con = "script.R")

  # stub crandb lookup so we don't touch the network
  renv_scope_binding(
    envir = asNamespace("renv"),
    symbol = "renv_sysreqs_crandb",
    replacement = function(package, version = NULL) {
      list(Package = package, Version = version, SystemRequirements = "zlib")
    }
  )

  # lockfile packages and project dependencies are used by default
  sysdeps <- sysreqs(project = project, distro = "ubuntu:24.04", report = FALSE)
  expect_true(all(c("morning", "evening", "night", "toast") %in% names(sysdeps)))
  expect_equal(sysdeps$morning$packages, list("libcurl4-openssl-dev"))
  expect_equal(sysdeps$evening$packages, list("libfreetype6-dev", "libpng-dev"))
  expect_null(sysdeps$night)
  expect_equal(sysdeps$toast$packages, list("zlib1g-dev"))

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

test_that("sysreqs() includes recursive dependencies", {

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
        Imports = list("evening (>= 1.0.0)", "utils")
      ),
      evening = list(
        Package = "evening",
        Version = "1.0.0",
        Source = "Repository",
        Repository = "CRAN",
        Title = "A Test Package",
        LinkingTo = list("night"),
        SystemRequirements = "freetype2, libpng"
      ),
      night = list(
        Package = "night",
        Version = "1.0.0",
        Source = "Repository",
        Repository = "CRAN",
        Title = "A Test Package",
        SystemRequirements = "libcurl (>= 7.62)"
      )
    )
  )

  renv_json_write(lockfile, file = "renv.lock")

  sysdeps <- sysreqs(
    packages = "morning",
    source   = "lockfile",
    report   = FALSE,
    distro   = "ubuntu:24.04",
    project  = project
  )

  expect_equal(names(sysdeps), c("morning", "evening", "night"))
  expect_null(sysdeps$morning)
  expect_equal(sysdeps$evening$packages, list("libfreetype6-dev", "libpng-dev"))
  expect_equal(sysdeps$night$packages, list("libcurl4-openssl-dev"))

  sysdeps <- sysreqs(
    packages  = "morning",
    source    = "lockfile",
    recursive = FALSE,
    report    = FALSE,
    distro    = "ubuntu:24.04",
    project   = project
  )

  expect_equal(names(sysdeps), "morning")

})

test_that("recursive dependencies are specific to the package version", {

  lockfile <- list(
    morning = list(Package = "morning", Version = "1.0.0", Title = "A Test Package", Imports = "evening"),
    evening = list(Package = "evening", Version = "1.0.0", Title = "A Test Package"),
    night   = list(Package = "night", Version = "1.0.0", Title = "A Test Package")
  )

  records <- renv_sysreqs_records("morning", "lockfile", lockfile, recursive = TRUE)
  expect_equal(names(records), c("morning", "evening"))

  # a different version of the same package can have different dependencies
  lockfile$morning$Version <- "2.0.0"
  lockfile$morning$Imports <- "night"

  records <- renv_sysreqs_records("morning", "lockfile", lockfile, recursive = TRUE)
  expect_equal(names(records), c("morning", "night"))

})

test_that("recursive resolution tolerates cycles and unresolved packages", {

  lockfile <- list(
    morning = list(Package = "morning", Version = "1.0.0", Title = "A Test Package", Imports = "evening, missing"),
    evening = list(Package = "evening", Version = "1.0.0", Title = "A Test Package", Imports = "morning")
  )

  records <- renv_sysreqs_records("morning", "lockfile", lockfile, recursive = TRUE)
  expect_equal(names(records), c("morning", "evening", "missing"))
  expect_null(records$missing)

})

test_that("crandb lookups use the version available from the repositories", {

  renv_tests_scope()

  expect_equal(renv_sysreqs_version("bread"), "1.0.0")
  expect_null(renv_sysreqs_version("no.such.package"))

  # stub crandb lookup so we don't touch the network
  requested <- NULL
  renv_scope_binding(
    envir = asNamespace("renv"),
    symbol = "renv_sysreqs_crandb",
    replacement = function(package, version = NULL) {
      requested <<- version
      list(Package = package, Version = version)
    }
  )

  renv_sysreqs_lookup("bread", sources = "crandb", lockfile = NULL)
  expect_equal(requested, "1.0.0")

  # a version recorded in the lockfile takes precedence
  lockfile <- list(
    bread = list(
      Package = "bread",
      Version = "0.1.0",
      Source = "Repository",
      Repository = "CRAN",
      Hash = "0123456789abcdef"
    )
  )

  renv_sysreqs_lookup("bread", sources = c("lockfile", "crandb"), lockfile = lockfile)
  expect_equal(requested, "0.1.0")

})

test_that("dependencies reported by crandb are converted", {

  json <- '{
    "Package": "morning",
    "Version": "1.0.0",
    "Depends": {"R": ">= 3.5.0"},
    "Imports": {"evening": ">= 1.0.0", "night": "*"},
    "SystemRequirements": "libcurl"
  }'

  # stub download so we don't touch the network
  renv_scope_binding(
    envir = asNamespace("renv"),
    symbol = "download",
    replacement = function(url, destfile, ...) writeLines(json, con = destfile)
  )

  record <- renv_sysreqs_crandb_impl_one("morning", "1.0.0")
  expect_equal(record$Imports, "evening (>= 1.0.0), night")
  expect_equal(record$SystemRequirements, "libcurl")
  expect_equal(renv_graph_deps(record), c("evening", "night"))

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

  # the requirements of these packages' dependencies change over time
  packages <- c("magick", "tesseract")
  expect_snapshot(
    . <- sysreqs(packages, source = "crandb", recursive = FALSE, distro = "redhat:8"),
    transform = function(x) gsub("\\[\\d+/\\d+\\] ", "", x)
  )

})
