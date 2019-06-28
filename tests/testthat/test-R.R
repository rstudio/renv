
context("R")

test_that("we can use R CMD build to build a package", {

  testdir <- tempfile("renv-r-tests-")
  ensure_directory(testdir)
  owd <- setwd(testdir)
  on.exit(setwd(owd), add = TRUE)

  package <- "sample.package"
  pkgdir <- file.path(testdir, package)
  ensure_directory(pkgdir)

  data <- list(Package = package, Type = "Package", Version = "0.1.0")
  write.dcf(data, file = file.path(pkgdir, "DESCRIPTION"))
  expect_equal(renv_project_type(pkgdir), "package")

  tarball <- r_cmd_build(package, pkgdir)
  ensure_existing_path(tarball)
  ensure_existing_file(tarball)
  expect_true(file.exists(tarball))
  files <- renv_archive_list(tarball)
  expect_true(all(c("DESCRIPTION", "NAMESPACE", "MD5") %in% basename(files)))

  before <- list.files(testdir)
  args <- c("CMD", "INSTALL", "--build", package)
  output <- r_exec(package, args, "build")
  after <- list.files(testdir)
  binball <- setdiff(after, before)

  expect_true(length(binball) == 1)
  expect_equal(renv_package_type(binball), "binary")

})
