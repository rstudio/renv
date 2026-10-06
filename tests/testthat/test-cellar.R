
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

test_that("cellar packages using other tar compressions are found", {
  skip_on_cran()
  renv_tests_scope()

  cellar <- renv_paths_cellar()
  ensure_directory(cellar)

  pkgdir <- renv_scope_tempfile("renv-cellar-pkg-")
  ensure_directory(file.path(pkgdir, "xzpkg"))
  writeLines(
    c(
      "Package: xzpkg",
      "Version: 1.0.0",
      "Title: A Cellar Package",
      "Description: Test.",
      "License: MIT"
    ),
    file.path(pkgdir, "xzpkg", "DESCRIPTION")
  )

  renv_scope_wd(pkgdir)

  # a binary built for some other build of R must be ignored
  tar(
    tarfile     = file.path(cellar, "xzpkg_1.0.0_R_other-build.tar.xz"),
    files       = "xzpkg",
    compression = "xz"
  )

  record <- list(Package = "xzpkg", Version = "1.0.0")
  expect_error(renv_retrieve_cellar_find(record), "not available locally")

  # ... whatever its compression, and the cellar listing must not advertise it
  tar(
    tarfile     = file.path(cellar, "xzpkg_1.0.0_R_other-build.tar.gz"),
    files       = "xzpkg",
    compression = "gzip"
  )

  expect_error(renv_retrieve_cellar_find(record), "not available locally")
  listing <- renv_available_packages_cellar("source")
  expect_false("xzpkg" %in% listing$Package)

  # an archive without a build designation is accepted whatever its compression
  path <- file.path(cellar, "xzpkg_1.0.0.tar.xz")
  tar(tarfile = path, files = "xzpkg", compression = "xz")

  found <- renv_retrieve_cellar_find(record)
  expect_equal(unname(found), path)
  expect_equal(names(found), "source")

})

test_that("cellar binaries built for this build of R are advertised", {
  skip_on_cran()
  renv_tests_scope()

  cellar <- renv_paths_cellar()
  ensure_directory(cellar)

  # pretend this build of R writes a known build designation
  renv_scope_binding(
    envir       = asNamespace("renv"),
    symbol      = "renv_pkgtype_build",
    replacement = function(type = NULL) "test-build"
  )

  pkgdir <- renv_scope_tempfile("renv-cellar-pkg-")
  ensure_directory(file.path(pkgdir, "buildpkg"))
  writeLines(
    c(
      "Package: buildpkg",
      "Version: 1.0.0",
      "Title: A Cellar Package",
      "Description: Test.",
      "License: MIT"
    ),
    file.path(pkgdir, "buildpkg", "DESCRIPTION")
  )

  renv_scope_wd(pkgdir)
  path <- file.path(cellar, "buildpkg_1.0.0_R_test-build.tar.gz")
  tar(tarfile = path, files = "buildpkg", compression = "gzip")

  # the lookup accepts the archive ...
  record <- list(Package = "buildpkg", Version = "1.0.0")
  found <- renv_retrieve_cellar_find(record)
  expect_equal(unname(found), path)

  # ... the listing advertises it under its real file name ...
  renv_scope_options(repos = character())
  listing <- renv_available_packages_cellar("source")
  expect_true(basename(path) %in% listing$File)

  # ... and the parallel download path resolves to that file
  latest <- renv_available_packages_latest("buildpkg", type = "source")
  url <- renv_graph_url_cellar(latest)
  expect_equal(basename(url$url), basename(path))
  expect_true(file.exists(renv_url_local_path(url$url)))

})
