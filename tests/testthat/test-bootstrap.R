
test_that("we can bootstrap the current version of renv", {

  skip_on_cran()
  skip_on_ci()

  renv_tests_scope()

  library <- renv_libpaths_active()
  bootstrap(version = "1.0.0", library = library)
  expect_true(renv_package_installed("renv", library))
  expect_true(renv_package_version("renv") == "1.0.0")

})

test_that("we can bootstrap an archived version of renv", {

  skip_on_cran()
  skip_on_ci()

  renv_tests_scope()

  library <- renv_libpaths_active()
  bootstrap(version = "0.1.0", library = library)
  expect_true(renv_package_installed("renv", library))
  expect_true(renv_package_version("renv") == "0.1.0")

})

test_that("we can install a version of renv with a sha", {

  skip_on_cran()
  skip_on_ci()

  renv_tests_scope()

  version <- structure("0.17.3-62", sha = "5049cef8a")
  library <- renv_libpaths_active()
  bootstrap(version = version, library = library)
  expect_true(renv_package_installed("renv", library))

  desc <- utils::packageDescription("renv", library)
  expect_equal(desc$RemoteType, "github")
  expect_equal(desc$RemotePkgRef, "rstudio/renv")
  expect_equal(desc$RemoteSha, "5049cef8a94591b802f9766a0da092780f59f7e4")
})

test_that("bootstrap functions don't depend on non-bootstrap APIs", {

  # pattern matching things that are bootstrapped for renv
  pattern <- "^renv_(?:bootstrap|json|options|remote)_"

  # get all of the bootstrap functions defined in renv
  renv <- asNamespace("renv")
  keys <- grep(pattern, ls(envir = renv), value = TRUE)
  fns <- mget(keys, envir = renv, mode = "function")
  bodies <- map(fns, body)

  # iterate over those functions and look for the called functions
  calls <- stack(mode = "character")
  recurse(bodies, function(node) {
    if (is.call(node) && is.symbol(node[[1L]]))
      calls$push(as.character(node[[1L]]))
  })
  calls <- calls$data()

  # check what renv APIs are used
  apis <- grep("^renv_", calls, value = TRUE)

  # validate they're all available
  bad <- grep(pattern, apis, value = TRUE, invert = TRUE)
  expect_true(length(bad) == 0, info = paste("Functions:", bad, collapse = ", "))

})

test_that("bootstrapping functions standalone", {

  renv_tests_scope()

  # get all bootstrap APIs in package
  renv <- asNamespace("renv")
  keys <- ls(envir = renv, pattern = "^renv_bootstrap_", all.names = TRUE)
  vals <- mget(c("catf", "%||%", "header", "bootstrap", keys), envir = renv)

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

test_that("bootstrapping gives informative output when succesful", {
  env <- environment()

  local_mocked_bindings(
    renv_bootstrap_download_impl = function(url, destfile) {
      file.create(destfile)
      defer(unlink(destfile), env)
      0L
    },
    renv_bootstrap_install_impl = function(cmd, args, ...) {
      structure("", status = 0L)
    },
    renv_bootstrap_download_cran_latest_find = function(version) {
      if (package_version(version) < "1.0.0") {
        list(type = "binary", repos = "https://cran.rstudio.com")
      }
    },
    load = function(...) invisible()
  )

  renv_scope_options(renv.bootstrap.quiet = FALSE)
  library <- renv_scope_tempfile()

  expect_snapshot({
    bootstrap(version = "0.9.0", library = library)
    bootstrap(version = "1.0.0", library = library)
    bootstrap(version = "1.0.0.1", library = library)
  })
})

test_that("bootstrapping gives informative output when download fails", {
  local_mocked_bindings(
    renv_bootstrap_download_impl = function(...) {
      stop("test failure")
    },
    renv_bootstrap_download_cran_latest_find = function(version) {
      if (package_version(version) < "1.0.0") {
        list(type = "binary", repos = "https://cran.rstudio.com")
      }
    }
  )
  renv_scope_options(renv.bootstrap.quiet = FALSE)
  library <- renv_scope_tempfile()

  expect_snapshot(error = TRUE, {
    bootstrap(version = "0.9.0", library = library)
    bootstrap(version = "1.0.0", library = library)
    bootstrap(version = "1.0.0.1", library = library)
  })
})


test_that("bootstrapping gives informative output when install fails", {
  local_mocked_bindings(
    renv_bootstrap_download_impl = function(url, destfile) {
      file.create(destfile)
      0L
    },
    renv_bootstrap_install_impl = function(...) {
      structure("test failure", status = 123L)
    }
  )
  renv_scope_options(renv.bootstrap.quiet = FALSE)
  library <- renv_scope_tempfile()

  expect_snapshot(bootstrap(version = "1.0.0.1", library = library), error = TRUE)

})



# helpers -----------------------------------------------------------------

test_that("renv_boostrap_version_validate() recognises when versions are the same", {

  expect_true(
    renv_bootstrap_validate_version(
      version = structure("1.2.3", sha = "abcd123"),
      description = list(RemoteSha = "abcd1234567")
    )
  )

  expect_true(
    renv_bootstrap_validate_version(
      version = "1.2.3",
      description = list(Version = "1.2.3")
    )
  )

})


test_that("renv_boostrap_version_validate() gives good warnings", {
  renv_scope_options(renv.bootstrap.quiet = FALSE)

  expect_snapshot({

    # out-of-sync (request release; have different release)
    . <- renv_bootstrap_validate_version(
      version = "1.2.3",
      description = list(Version = "2.3.4")
    )

    # out-of-sync (request dev; have release)
    . <- renv_bootstrap_validate_version(
      version = "1.2.3-1",
      description = list(Version = "1.2.3")
    )

    # out-of-sync (request dev; have different dev)
    . <- renv_bootstrap_validate_version(
      version = structure("1.2.3-1", sha = "22d015905828c3398728a5ff9e381e0433976263"),
      description = list(
        Version = "1.2.3-1",
        RemoteType = "github",
        RemoteSha = "6b09befaaba3f55e0e2c141cb45c5d247b61ef1e"
      )
    )

    # out-of-sync (request dev; have release)
    . <- renv_bootstrap_validate_version(
      version = structure("1.2.3-1", sha = "22d015905828c3398728a5ff9e381e0433976263"),
      description = list(
        Version = "1.2.3"
      )
    )

  })

})

test_that("bootstrap version validation handles 'standard' remote types", {

  renv_scope_options(renv.bootstrap.quiet = FALSE)
  expect_snapshot(

    . <- renv_bootstrap_validate_version(
      version = "1.0.0",
      description = list(
        Version = "1.0.1",
        RemoteType = "standard",
        RemoteSha = "1.0.1"
      )
    )

  )

})
