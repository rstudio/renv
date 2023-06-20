
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

  BiocManager <- asNamespace("BiocManager")
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

  local({
    renv_tests_scope_system_cache()
    install("bioc::Biobase", rebuild = TRUE)
  })

  expect_true(renv_package_installed("Biobase"))

})

test_that("bioconductor.version can be used to freeze version", {

  project <- renv_tests_scope()

  settings$bioconductor.version("3.14", project = project)
  expect_equal(renv_bioconductor_version(project = project), "3.14")

})

test_that("we can restore a lockfile using multiple Bioconductor releases", {

  skip_on_cran()
  skip_on_windows()
  skip_if(getRversion()[1, 1:2] != "4.1")
  skip_if_not_installed("BiocManager")

  project <- renv_tests_scope()

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

  # create a dummy package which has a Bioconductor dependency
  pkgdir <- file.path(tempdir(), "bioc.example")
  ensure_directory(pkgdir)

  desc <- heredoc("
    Package: bioc.example
    Version: 1.0.0
    Imports: Biobase
    biocViews: Biology
  ")
  writeLines(desc, con = file.path(tempdir(), "bioc.example/DESCRIPTION"))

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

  expect_snapshot(
    install("bioc::BiocGenerics"),
    transform = function(x) strip_versions(strip_dirs(x))
  )

  expect_true(renv_package_installed("BiocManager"))

})
