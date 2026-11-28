
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

test_that("packages can be installed, restored from Bioconductor", {

  skip_slow()
  skip_on_os("windows")
  skip_if(getRversion() < "3.5.0")
  skip_if(R.version$nickname == "Unsuffered Consequences")
  skip_if_no_bioconductor()

  renv_tests_scope("Biobase")
  local({
    renv_tests_scope_system_cache()
    install("BiocManager")
  })

  local({
    renv_scope_options(repos = c(CRAN = "https://cran.r-project.org"))
    suppressMessages(
      BiocManager::install(
        pkgs   = "Biobase",
        quiet  = TRUE,
        update = FALSE,
        ask    = FALSE
      )
    )
  })

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
  skip_if_no_bioconductor()

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

  skip_if_no_bioconductor()

  renv_tests_scope()
  defer(unloadNamespace("BiocManager"))

  init()

  local({
    renv_tests_scope_system_cache()
    install("bioc::BiocGenerics")
  })

  snapshot()
  writeLines("library(BiocGenerics)", "dependencies.R")

  lockfile <- snapshot()
  expect_contains(names(lockfile$Packages), "BiocManager")

  # And it goes away when we remove the dependency
  unlink("dependencies.R")
  lockfile <- snapshot()
  records <- renv_lockfile_records(lockfile)
  expect_length(records, 0L)

})

test_that("auto-bioc install happens silently", {

  # https://github.com/rstudio/renv/actions/runs/5326472190/jobs/9648557761#step:6:295
  skip_if(renv_platform_windows())
  skip_if_no_bioconductor()

  renv_tests_scope()
  renv_tests_scope_system_cache()
  defer(unloadNamespace("BiocManager"))

  install("bioc::BiocGenerics")
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
    Repository   = "Bioconductor 3.18"
  )

  expect_identical(actual[names(expected)], expected)

})

# https://github.com/rstudio/renv/issues/1902
test_that("BiocManager is installed at the version recorded in the lockfile", {

  skip_if(getRversion() < "3.5.0")
  renv_tests_scope()

  args <- NULL
  local_mocked_bindings(
    install = function(packages, ...) { args <<- packages; invisible(NULL) }
  )

  library <- renv_scope_tempfile("renv-library-")
  record <- list(
    Package    = "BiocManager",
    Version    = "1.30.22",
    Source     = "Repository",
    Repository = "CRAN"
  )

  # without a lockfile, the latest version of BiocManager is requested, which
  # requires querying the active package repositories
  renv_bioconductor_init(library = library)
  expect_equal(args, "BiocManager")

  # with one, we install the recorded version, which can be found in the cache
  lockfile <- list(Packages = list(BiocManager = record))
  renv_bioconductor_init(library = library, lockfile = lockfile)
  expect_equal(args, list(BiocManager = record))

  # a lockfile which doesn't record BiocManager is no different
  lockfile <- list(Packages = list(bread = list(Package = "bread", Version = "1.0.0")))
  renv_bioconductor_init(library = library, lockfile = lockfile)
  expect_equal(args, "BiocManager")

})

test_that("a recorded package can be installed from the cache without repositories", {

  skip_on_cran()
  skip_on_windows()
  renv_tests_scope("bread")
  init()

  lockfile <- renv_lockfile_read("renv.lock")
  record <- lockfile$Packages$bread

  # with no reachable repositories, only the cache can supply the package
  renv_scope_options(repos = c(CRAN = "file:///nonexistent"))
  library <- renv_scope_tempfile("renv-library-")
  ensure_directory(library)
  quietly(install(list(bread = record), library = library, prompt = FALSE))
  expect_true(renv_package_installed("bread", lib.loc = library))

})

test_that("restore() initializes Bioconductor using the lockfile", {

  skip_if(getRversion() < "3.5.0")
  renv_tests_scope()

  lockfile <- list(
    R = list(Version = format(getRversion()), Repositories = list()),
    Bioconductor = list(Version = "3.18"),
    Packages = list(
      BiocManager = list(
        Package    = "BiocManager",
        Version    = "1.30.22",
        Source     = "Repository",
        Repository = "CRAN"
      )
    )
  )

  args <- NULL
  local_mocked_bindings(
    renv_bioconductor_init = function(library = NULL, lockfile = NULL) {
      args <<- lockfile
      stop("initialized")
    }
  )

  expect_error(restore(lockfile = lockfile, prompt = FALSE), "initialized")
  expect_equal(args$Packages$BiocManager$Version, "1.30.22")

})

test_that("load() initializes Bioconductor using the lockfile", {

  skip_if(getRversion() < "3.5.0")
  project <- renv_tests_scope()
  renv_scope_options(
    repos = getOption("repos"),
    renv.bioconductor.repos =c(BioCsoft = "https://bioconductor.org/packages/3.18/bioc"),
    renv.bioconductor.validate = FALSE
  )

  args <- NULL
  local_mocked_bindings(
    renv_bioconductor_init = function(library = NULL, lockfile = NULL) {
      args <<- lockfile
    }
  )

  lockfile <- list(
    Bioconductor = list(Version = "3.18"),
    Packages = list(BiocManager = list(Package = "BiocManager", Version = "1.30.22"))
  )

  renv_load_bioconductor(project, lockfile$Bioconductor, lockfile)
  expect_equal(args$Packages$BiocManager$Version, "1.30.22")

})
