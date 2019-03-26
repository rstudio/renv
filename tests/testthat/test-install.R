context("Install")

test_that("installation failure is well-reported", {

  owd <- setwd(tempdir())
  on.exit(setwd(owd), add = TRUE)

  # init dummy library
  library <- tempfile("renv-library-")
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
