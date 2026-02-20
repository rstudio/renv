
test_that("use() works as intended", {

  skip_on_cran()

  renv_tests_scope()
  init()

  oldpaths <- .libPaths()

  use(
    package = "toast",
    isolate = FALSE,
    attach  = FALSE,
    verbose = FALSE,
    sandbox = FALSE
  )

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

test_that("renv_use_cacheonly_install installs packages from the cache", {

  skip_on_cran()

  renv_tests_scope("bread")
  init()

  # get records from the lockfile (has proper Source, Version, etc.)
  lockfile <- renv_lockfile_read("renv.lock")
  records <- renv_lockfile_records(lockfile)

  # set up a fresh library and install from cache
  library <- tempfile("renv-library-")
  ensure_directory(library)

  installed <- renv_use_cacheonly_install(records = records, library = library)
  expect_true("bread" %in% names(installed))
  expect_true(file.exists(file.path(library, "bread")))

})

test_that("renv_use_cacheonly_install skips packages not in the cache", {

  skip_on_cran()

  renv_tests_scope()

  # set up a fresh library
  library <- tempfile("renv-library-")
  ensure_directory(library)

  # use a record for a package that won't be in the cache
  records <- list(
    nosuchpackage = list(
      Package = "nosuchpackage",
      Version = "1.0.0",
      Source  = "Repository"
    )
  )

  installed <- renv_use_cacheonly_install(records = records, library = library)
  expect_length(installed, 0)
  expect_false(file.exists(file.path(library, "nosuchpackage")))

})

test_that("renv_use_cacheonly_restore installs lockfile packages from cache", {

  skip_on_cran()
  skip_on_windows()

  renv_tests_scope("bread")
  init()

  # set up a fresh library
  library <- tempfile("renv-library-")
  ensure_directory(library)

  installed <- renv_use_cacheonly_restore(lockfile = "renv.lock", library = library)
  expect_true("bread" %in% names(installed))
  expect_true(file.exists(file.path(library, "bread")))

})

test_that("use(repos = NULL) installs from cache only", {

  skip_on_cran()

  renv_tests_scope("bread")
  init()

  # reset the shared use libpath so we get a fresh library
  renv_scope_binding(the, "use_libpath", NULL)
  renv_scope_libpaths()

  use(
    "bread",
    repos    = NULL,
    isolate  = TRUE,
    verbose  = FALSE,
    sandbox  = FALSE
  )

  libpath <- renv_use_libpath()
  expect_true(file.exists(file.path(libpath, "bread")))

})
