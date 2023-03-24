
context("Bioconductor")

test_that("packages can be installed, restored from Bioconductor", {

  skip_on_cran()
  skip_on_os("windows")
  skip_if(getRversion() < "3.5.0")
  skip_if(R.version$nickname == "Unsuffered Consequences")

  renv_scope_options(renv.tests.verbose = FALSE)

  renv_tests_scope("Biobase")

  cran <- "https://cloud.r-project.org"
  install.packages("BiocManager", repos = cran, quiet = TRUE)
  BiocManager <- asNamespace("BiocManager")

  # TODO: Running this directly from 'renv' seems to fail, at least
  # on R 4.2-arm64 macOS?
  local({
    renv_scope_envvars(R_LIBS = .libPaths()[1])

    lines <- c(
      sprintf(".libPaths('%s')", .libPaths()[1L]),
      "options(pkgType = 'source')",
      "BiocManager::install('Biobase', quiet = TRUE)"
    )

    code <- paste(lines, collapse = "; ")
    args <- c("--vanilla", "-s", "-e", renv_shell_quote(code))
    renv_system_exec(R(), args, action = "installing Biobase", quiet = TRUE)
  })

  expect_true(renv_package_installed("BiocManager"))
  expect_true(renv_package_installed("BiocVersion"))
  expect_true(renv_package_installed("Biobase"))

  snapshot()

  lockfile <- snapshot(lockfile = NULL)
  expect_true("Bioconductor" %in% names(lockfile))
  expect_equal(names(lockfile)[2], "Bioconductor")
  expect_equal(lockfile$Bioconductor$Version, format(BiocManager$version()))

  records <- renv_lockfile_records(lockfile)
  expect_true("BiocManager" %in% names(records))
  expect_true("BiocVersion" %in% names(records))
  expect_true("Biobase" %in% names(records))

  remove("Biobase")
  restore()
  expect_true(renv_package_installed("Biobase"))

})

test_that("renv::install(<bioc>, rebuild = TRUE) works", {

  skip_on_cran()
  skip_on_os("windows")
  skip_if(getRversion() < "3.5.0")
  skip_if(R.version$nickname == "Unsuffered Consequences")
  skip_if_not_installed("BiocManager")

  renv_scope_options(renv.tests.verbose = FALSE)

  requireNamespace("BiocManager", quietly = TRUE)
  on.exit(unloadNamespace("BiocManager"), add = TRUE)

  renv_tests_scope()
  install("bioc::Biobase", rebuild = TRUE)

  expect_true(renv_package_installed("Biobase"))

})

test_that("bioconductor.version can be used to freeze version", {

  skip_on_cran()
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

  path <- file.path(renv_tests_root(), "resources/bioconductor.lock")
  lockfile <- renv_lockfile_read(path)

  status <- restore(
    lockfile = lockfile,
    packages = c("limma", "BiocGenerics"),
    rebuild  = TRUE
  )

  expect_true(renv_package_version("limma") == "3.50.0")
  expect_true(renv_package_version("BiocGenerics") == "0.38.0")

})
