
context("Bootstrap")

test_that("we can bootstrap the current version of renv", {

  skip_on_cran()
  skip_on_ci()

  renv_tests_scope()

  library <- renv_libpaths_default()
  bootstrap(version = "1.0.0", library = library)
  expect_true(renv_package_installed("renv", library))
  expect_true(renv_package_version("renv") == "1.0.0")

})

test_that("we can bootstrap an archived version of renv", {

  skip_on_cran()
  skip_on_ci()

  renv_tests_scope()

  library <- renv_libpaths_default()
  bootstrap(version = "0.1.0", library = library)
  expect_true(renv_package_installed("renv", library))
  expect_true(renv_package_version("renv") == "0.1.0")

})

test_that("we can install a version of renv from GitHub", {

  skip_on_cran()
  skip_on_ci()

  renv_tests_scope()

  library <- renv_libpaths_default()
  bootstrap(version = "0.12.3-1", library = library)
  expect_true(renv_package_installed("renv", library))
  expect_true(renv_package_version("renv") == "0.12.3-1")

})

test_that("bootstrap succeeds with empty repos", {

  skip_on_cran()
  skip_on_os("windows")

  renv_tests_scope()
  renv_scope_options(repos = character())

  library <- renv_libpaths_default()
  bootstrap(version = "1.0.0", library = library)
  expect_true(renv_package_installed("renv", library))
  expect_true(renv_package_version("renv") == "1.0.0")

})

test_that("bootstrap functions don't depend on non-bootstrap APIs", {

  # get all of the bootstrap functions defined in renv
  renv <- asNamespace("renv")
  keys <- grep("^renv_bootstrap_", ls(envir = renv), value = TRUE)
  fns <- mget(keys, envir = renv, mode = "function")
  bodies <- map(fns, body)

  # iterate over those functions and look for the called functions
  calls <- stack(mode = "character")
  recurse(bodies, function(node, stack) {
    if (is.call(node) && is.symbol(node[[1L]]))
      calls$push(as.character(node[[1L]]))
  })
  calls <- calls$data()

  # check what renv APIs are used
  apis <- grep("^renv_", calls, value = TRUE)

  # validate they're all renv_bootstrap_ APIs
  ok <- grepl("^renv_bootstrap_", apis)
  expect_true(all(ok))

})

test_that("bootstrapping functions standalone", {

  renv_tests_scope()

  # get all bootstrap APIs in package
  renv <- asNamespace("renv")
  keys <- ls(envir = renv, pattern = "^renv_bootstrap_", all.names = TRUE)
  vals <- mget(c("bootstrap", keys), envir = renv)

  # put those into a separate environment inheriting only from base, and
  # re-mark those as inheriting from base (so they only 'see' each-other)
  envir <- new.env(parent = baseenv())
  for (i in seq_along(vals))
    environment(vals[[i]]) <- envir
  list2env(vals, envir = envir)

  # set up library path
  library <- renv_scope_tempfile("renv-library-")
  ensure_directory(library)

  # try running 'sandboxed' version of bootstrap
  run <- function(version) {

    # construct bootstrap call
    code <- call("bootstrap", version = version, library = library)

    # run that call
    eval(code, envir = envir)

    # validate that renv was successfully installed
    expect_true(renv_package_installed("renv", lib.loc = library))

    # validate that the correct version was installed
    descpath <- file.path(library, "renv/DESCRIPTION")
    desc <- renv_description_read(path = descpath)
    expect_identical(desc$Package, "renv")
    expect_identical(desc$Version, version)

  }

  # current on CRAN (test repository version)
  run("1.0.0")

  # in the CRAN archive (test repository version)
  run("0.1.0")

})
