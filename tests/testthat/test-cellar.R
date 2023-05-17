
context("Cellar")

test_that("renv can find packages located in the cellar", {
  skip_on_cran()
  renv_tests_scope()

  # copy some packages into the cellar
  cellar <- renv_paths_cellar()
  ensure_directory(cellar)

  repopath <- getOption("renv.tests.repopath")
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
  renv::install("bread")

})
