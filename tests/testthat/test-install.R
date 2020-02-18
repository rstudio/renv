context("Install")

test_that("install works when DESCRIPTION contains no dependencies", {
  renv_tests_scope()
  desc <- c("Type: Package", "Package: test")
  writeLines(desc, con = "DESCRIPTION")
  expect_length(install(), 0L)
})

test_that("requested version in DESCRIPTION file is honored", {

  renv_tests_scope()

  desc <- c(
    "Type: Package",
    "Package: test",
    "Imports: bread (== 0.1.0), toast"
  )
  writeLines(desc, con = "DESCRIPTION")

  install()

  expect_true(renv_package_version("bread") == "0.1.0")

})

test_that("installation failure is well-reported", {

  # TODO: test seems to fail because a connection gets
  # left open by utils::package.skeleton()
  skip_on_os("windows")

  owd <- setwd(tempdir())
  on.exit(setwd(owd), add = TRUE)

  # init dummy library
  library <- renv_tempfile("renv-library-")
  ensure_directory(library)

  # dummy environment
  envir <- new.env(parent = emptyenv())
  envir[["hello"]] <- function() {}

  # prepare dummy package
  package <- "renv.dummy.package"
  unlink(package, recursive = TRUE)
  utils::package.skeleton(package, environment = envir)

  # remove broken man files
  unlink("renv.dummy.package/Read-and-delete-me")
  unlink("renv.dummy.package/man", recursive = TRUE)

  # give the package a build-time error
  writeLines("parse error", con = file.path(package, "R/error.R"))

  # try to build it and confirm error
  renv_scope_options(renv.verbose = FALSE)
  expect_error(renv_install_package_local_impl(package, package, library))

})

test_that("install forces update of dependencies as needed", {

  # TODO: this fails on CRAN presumedly because the wrong
  # version of the breakfast package is searched for; need
  # to figure out where the repositories are getting changed.
  skip_on_cran()
  renv_tests_scope("breakfast")

  # install the breakfast package
  renv::install("breakfast")

  # ensure its dependencies were installed
  packages <- c("bread", "oatmeal", "toast")
  for (package in packages)
    expect_true(file.exists(renv_package_find(package)))

  # remove breakfast
  renv::remove("breakfast")

  # modify 'toast' so that it's now too old
  path <- renv_package_find("toast")
  descpath <- file.path(path, "DESCRIPTION")
  desc <- renv_description_read(descpath)
  desc$Version <- "0.1.0"
  renv_dcf_write(desc, file = descpath)

  # try to install 'breakfast' again
  renv::install("breakfast")

  # validate that 'toast' was updated to 1.0.0
  desc <- renv_description_read(package = "toast")
  expect_equal(desc$Version, "1.0.0")

})

test_that("packages can be installed from local sources", {

  renv_tests_scope()
  renv::init()

  # get path to package sources in local repos
  repos <- getOption("renv.tests.repos")
  tarball <- file.path(repos, "src/contrib/bread_1.0.0.tar.gz")

  # try to install it
  renv::install(tarball)
  expect_true(renv_package_version("bread") == "1.0.0")

})

test_that("various remote styles can be used during install", {
  skip_on_cran()

  renv_tests_scope()
  renv::init()

  # install CRAN latest
  renv::install("bread")
  expect_true(renv_package_installed("bread"))
  expect_true(renv_package_version("bread") == "1.0.0")

  # install from archive
  renv::install("bread@0.1.0")
  expect_true(renv_package_installed("bread"))
  expect_true(renv_package_version("bread") == "0.1.0")

  # install from github
  renv::install("kevinushey/skeleton")
  expect_true(renv_package_installed("skeleton"))
  expect_true(renv_package_version("skeleton") == "1.0.1")

  # install from github PR
  renv::install("kevinushey/skeleton#1")
  expect_true(renv_package_installed("skeleton"))
  expect_true(renv_package_version("skeleton") == "1.0.2")

  # install from branch
  renv::install("kevinushey/skeleton@feature/version-bump")
  expect_true(renv_package_installed("skeleton"))
  expect_true(renv_package_version("skeleton") == "1.0.2")

  # install from subdir
  renv::install("kevinushey/subdir:subdir")
  expect_true(renv_package_installed("subdir"))
  expect_true(renv_package_version("subdir") == "0.0.0.9000")

  # install from URL to zip
  renv::install("https://github.com/kevinushey/skeleton/archive/master.zip")
  expect_true(renv_package_installed("skeleton"))
  expect_true(renv_package_version("skeleton") == "1.0.1")

})

test_that("Remotes fields in a project DESCRIPTION are respected", {
  skip_on_cran()

  renv_tests_scope()
  renv_scope_options(repos = character())
  renv::init()

  desc <- c(
    "Type: Package",
    "Package: renv.test.package",
    "Suggests: skeleton",
    "Remotes: kevinushey/skeleton"
  )

  writeLines(desc, con = "DESCRIPTION")
  renv::install()

  record <- renv_snapshot_description(package = "skeleton")
  expect_true(record$Source == "GitHub")

})

test_that("source packages in .zip files can be installed", {

  renv_tests_scope()

  dir <- tempfile("renv-ziptest-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  owd <- setwd(dir)
  on.exit(setwd(owd), add = TRUE)

  location <- download.packages("bread", destdir = tempdir())
  path <- location[1, 2]
  renv_archive_decompress(path, exdir = "bread")

  zippath <- file.path(getwd(), "bread_1.0.0.zip")
  setwd("bread")
  status <- catchall(zip(zippath, files = ".", extras = "-q"))
  setwd("..")

  if (inherits(status, "condition"))
    skip("could not zip archive")

  install(zippath)
  expect_true(renv_package_installed("bread"))

})

test_that("renv warns when installing an already-loaded package", {
  skip_on_cran()
  renv_tests_scope()
  install("bread@1.0.0")
  requireNamespace("bread")
  expect_condition(install("bread@0.1.0"), class = "renv.install.restart_required")
  unloadNamespace("bread")
})

test_that("renv::install() writes out Github fields for backwards compatibility", {
  skip_on_cran()
  renv_tests_scope()

  install("rstudio/packrat")
  descpath <- file.path(.libPaths()[1], "packrat/DESCRIPTION")
  dcf <- renv_description_read(descpath)

  expect_equal(dcf$RemoteRepo,     dcf$GithubRepo)
  expect_equal(dcf$RemoteUsername, dcf$GithubUsername)
  expect_equal(dcf$RemoteRef,      dcf$GithubRef)
  expect_equal(dcf$RemoteSha,      dcf$GithubSHA1)

})

test_that("renv uses safe library paths on Windows", {
  skip_if_not(renv_platform_windows())
  renv_tests_scope()

  goodlib <- "Research and Development"
  expect_true(renv_libpaths_safe(goodlib) == goodlib)

  badlib <- "R&D"
  expect_false(renv_libpaths_safe(badlib) != badlib)

  ensure_directory(badlib)
  renv_libpaths_set(badlib)
  install("bread")

  descpath <- file.path(getwd(), "R&D/bread")
  desc <- renv_description_read(descpath)

  expect_true(desc$Package == "bread")
  expect_true(desc$Version == "1.0.0")

})
