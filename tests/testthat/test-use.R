
test_that("use() works as intended", {

  skip_on_cran()

  renv_tests_scope()
  init()

  oldpaths <- .libPaths()

  use("toast", isolate = FALSE, attach = FALSE, verbose = FALSE, sandbox = FALSE)

  newpaths <- .libPaths()

  expect_true(length(newpaths) == length(oldpaths) + 1)

  toast <- find.package("toast")
  expect_true(renv_file_same(dirname(toast), .libPaths()[1]))

})

test_that("use(lockfile) works as intended", {

  skip_on_cran()
  skip_on_windows()

  renv_tests_scope("bread")
  init()

  renv_scope_libpaths()
  use(lockfile = "renv.lock", isolate = TRUE, verbose = FALSE, sandbox = FALSE)

  libpath <- renv_use_libpath()
  pkgpath <- renv_package_find("bread")

  expect_equal(
    renv_path_normalize(pkgpath),
    renv_path_normalize(file.path(libpath, "bread"))
  )

})
