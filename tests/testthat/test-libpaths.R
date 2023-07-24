test_that("resolve library with multiple fallbacks", {

  path <- tempfile()

  expect_equal(renv_libpaths_resolve(), .libPaths())
  expect_equal(renv_libpaths_resolve(path), c(path, .Library))

  dir.create(renv_paths_library(project = path), recursive = TRUE)
  expect_equal(
    renv_libpaths_resolve(NULL, path),
    c(renv_paths_library(project = path), .Library)
  )

})
