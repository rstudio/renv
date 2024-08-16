
# R 3.6 appears to have trouble if we try to load and unload
# different versions of BiocManager in the same session, so
# we need to use this hack to ensure that promises in the S3
# methods table are forced at opportune times
if (getRversion() < "4.0") {

  setHook(
    packageEvent("BiocManager", "onLoad"),
    function(...) renv_patch_methods_table()
  )

  setHook(
    packageEvent("BiocManager", "onUnload"),
    function(...) renv_patch_methods_table()
  )

}

# remove once Biobase can compile again
skip("wait for STRICT_R_HEADERS compliance")

test_that("packages can be installed, restored from Bioconductor", {

  skip_slow()
  skip_on_os("windows")
  skip_if(getRversion() < "3.5.0")
  skip_if(R.version$nickname == "Unsuffered Consequences")

  renv_tests_scope("Biobase")
  local({
    renv_tests_scope_system_cache()
    install("BiocManager")
  })
  suppressMessages(BiocManager::install("Biobase", quiet = TRUE, update = FALSE, ask = FALSE))

  expect_true(renv_package_installed("BiocManager"))
  expect_true(renv_package_installed("BiocVersion"))
  expect_true(renv_package_installed("Biobase"))

  snapshot(prompt = FALSE)

  lockfile <- snapshot(lockfile = NULL)
  expect_true("Bioconductor" %in% names(lockfile))

  BiocManager <- renv_scope_biocmanager()
  defer(unloadNamespace("BiocManager"))
  expect_equal(lockfile$Bioconductor$Version, format(BiocManager$version()))

  records <- renv_lockfile_records(lockfile)
  expect_true("BiocManager" %in% names(records))
  expect_true("BiocVersion" %in% names(records))
  expect_true("Biobase" %in% names(records))

  remove("Biobase")
  restore()
  expect_true(renv_package_installed("Biobase"))

})

test_that("install(<bioc>, rebuild = TRUE) works", {

  skip_on_cran()
  skip_on_os("windows")
  skip_if(getRversion() < "3.5.0")
  skip_if(R.version$nickname == "Unsuffered Consequences")
  skip_if_not_installed("BiocManager")

  renv_tests_scope()
  defer(unloadNamespace("BiocManager"))

  local({
    renv_tests_scope_system_cache()
    install("bioc::Biobase", rebuild = TRUE)
  })

  expect_true(renv_package_installed("Biobase"))

})

test_that("bioconductor.version can be used to freeze version", {

  project <- renv_tests_scope()
  defer(unloadNamespace("BiocManager"))

  settings$bioconductor.version("3.14", project = project)
  expect_equal(renv_bioconductor_version(project = project), "3.14")

})

test_that("we can restore a lockfile using multiple Bioconductor releases", {

  skip_on_cran()
  skip_on_windows()
  skip_if(getRversion()[1, 1:2] != "4.1")
  skip_if_not_installed("BiocManager")

  project <- renv_tests_scope()
  defer(unloadNamespace("BiocManager"))

  path <- renv_tests_path("resources/bioconductor.lock")
  lockfile <- renv_lockfile_read(path)

  status <- restore(
    lockfile = lockfile,
    packages = c("limma", "BiocGenerics"),
    rebuild  = TRUE
  )

  expect_true(renv_package_version("limma") == "3.50.0")
  expect_true(renv_package_version("BiocGenerics") == "0.38.0")

})

test_that("Bioconductor packages add BiocManager as a dependency", {

  renv_tests_scope()
  defer(unloadNamespace("BiocManager"))

  init()

  local({
    renv_tests_scope_system_cache()
    install("bioc::BiocGenerics")
  })

  snapshot()
  writeLines("library(BiocGenerics)", "dependencies.R")

  expect_snapshot(status(), transform = strip_versions)
  lockfile <- snapshot()
  expect_setequal(names(lockfile$Packages), c("BiocManager", "BiocGenerics", "BiocVersion"))

  # And it goes away when we remove the dependency
  unlink("dependencies.R")
  lockfile <- snapshot()
  records <- renv_lockfile_records(lockfile)
  expect_length(records, 0L)
  expect_snapshot(status())

})

test_that("remotes which depend on Bioconductor packages can be installed", {
  skip_on_cran()

  renv_tests_scope()
  renv_scope_options(repos = c(CRAN = "https://cloud.r-project.org"))
  defer(unloadNamespace("BiocManager"))

  # create a dummy package which has a Bioconductor dependency
  pkgdir <- file.path(tempdir(), "bioc.example")
  ensure_directory(pkgdir)

  desc <- heredoc("
    Package: bioc.example
    Version: 1.0.0
    Imports: Biobase
    biocViews: Biology
  ")
  writeLines(desc, con = file.path(pkgdir, "DESCRIPTION"))

  # try to install it
  local({
    renv_tests_scope_system_cache()
    install(pkgdir)
  })

  expect_true(renv_package_installed("Biobase"))
  expect_true(renv_package_installed("BiocGenerics"))

})


test_that("auto-bioc install happens silently", {

  # https://github.com/rstudio/renv/actions/runs/5326472190/jobs/9648557761#step:6:295
  skip_if(renv_platform_windows())

  renv_tests_scope()
  renv_tests_scope_system_cache()
  defer(unloadNamespace("BiocManager"))

  expect_snapshot(
    install("bioc::BiocGenerics"),
    transform = function(x) strip_versions(strip_dirs(x))
  )

  expect_true(renv_package_installed("BiocManager"))

})

test_that("standard bioc remotes are standardized appropriately", {

  contents <- heredoc('
    Package: BiocVersion
    Version: 3.18.1
    Title: Set the appropriate version of Bioconductor packages
    Description: This package provides repository information for the appropriate version of Bioconductor.
    Authors@R: c( person("Martin", "Morgan", email = "martin.morgan@roswellpark.org", role = "aut"), person("Marcel",
                 "Ramos", email = "marcel.ramos@roswellpark.org", role = "ctb"), person("Bioconductor", "Package
                 Maintainer", email = "maintainer@bioconductor.org", role = c("ctb", "cre")))
    biocViews: Infrastructure
    Depends: R (>= 4.3.0)
    License: Artistic-2.0
    Encoding: UTF-8
    RoxygenNote: 6.0.1
    git_url: https://git.bioconductor.org/packages/BiocVersion
    git_branch: RELEASE_3_18
    git_last_commit: 70680b8
    git_last_commit_date: 2023-11-15
    Repository: Bioconductor 3.18
    Date/Publication: 2023-11-18
    NeedsCompilation: no
    Packaged: 2023-11-18 19:15:45 UTC; biocbuild
    Author: Martin Morgan [aut], Marcel Ramos [ctb], Bioconductor Package Maintainer [ctb, cre]
    Maintainer: Bioconductor Package Maintainer <maintainer@bioconductor.org>
    Built: R 4.3.2; ; 2023-11-20 12:36:26 UTC; unix
    RemoteType: standard
    RemotePkgRef: BiocVersion
    RemoteRef: BiocVersion
    RemoteRepos: https://bioconductor.org/packages/3.18/bioc
    RemotePkgPlatform: aarch64-apple-darwin20
    RemoteSha: 3.18.1
  ')

  descfile <- tempfile("biocversion-")
  writeLines(contents, con = descfile)

  actual <- renv_snapshot_description(path = descfile)
  expected <- list(
    Package      = "BiocVersion",
    Version      = "3.18.1",
    Source       = "Bioconductor",
    Repository   = "Bioconductor 3.18",
    Requirements = "R",
    Hash         = "2ecaed86684f5fae76ed5530f9d29c4a"
  )

  expect_identical(actual, expected)

})
