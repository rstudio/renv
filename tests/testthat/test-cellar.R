
test_that("renv can find packages located in the cellar", {
  skip_on_cran()
  renv_tests_scope()

  # copy some packages into the cellar
  cellar <- renv_paths_cellar()
  ensure_directory(cellar)

  repopath <- renv_tests_repopath()
  packages <- list.files(
    path = file.path(repopath, "src/contrib"),
    pattern = ".tar.gz$",
    full.names = TRUE
  )

  file.copy(packages, to = cellar)

  # turn off repositories
  renv_scope_options(repos = character())

  # check for latest available package
  latest <- renv_available_packages_latest("bread", type = "source")
  expect_equal(
    latest[c("Package", "Version")],
    list(Package = "bread", Version = "1.0.0")
  )

  # check that we can install it
  install("bread")

})

# https://github.com/rstudio/renv/issues/2313
test_that("dependencies are resolved for packages installed from the cellar", {
  skip_on_cran()
  renv_tests_scope()

  # build a package that lives only in the cellar (not in the test
  # repositories), with a dependency on a package that does live in the
  # repositories; the cellar record must still expose that dependency
  cellar <- renv_paths_cellar()
  ensure_directory(cellar)

  pkgdir <- renv_scope_tempfile("renv-cellar-pkg-")
  ensure_directory(file.path(pkgdir, "cellarpkg"))
  writeLines(
    c(
      "Package: cellarpkg",
      "Version: 1.0.0",
      "Title: A Cellar Package",
      "Description: Test.",
      "Imports: bread",
      "License: MIT"
    ),
    file.path(pkgdir, "cellarpkg", "DESCRIPTION")
  )

  renv_scope_wd(pkgdir)
  tar(
    tarfile     = file.path(cellar, "cellarpkg_1.0.0.tar.gz"),
    files       = "cellarpkg",
    compression = "gzip"
  )

  descriptions <- renv_graph_init("cellarpkg")

  # cellarpkg should be resolved from the cellar ...
  source <- renv_record_source(descriptions[["cellarpkg"]], normalize = TRUE)
  expect_equal(source, "cellar")

  # ... and its dependency must still be discovered (#2313)
  expect_true("bread" %in% names(descriptions))

})
