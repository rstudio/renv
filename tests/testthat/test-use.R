
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

test_that("use(repos = NULL) resolves dependencies from cache", {

  skip_on_cran()

  renv_tests_scope("breakfast")
  init()

  # reset the shared use libpath so we get a fresh library
  renv_scope_binding(the, "use_libpath", NULL)
  renv_scope_libpaths()

  use(
    "breakfast",
    repos    = NULL,
    isolate  = TRUE,
    verbose  = FALSE,
    sandbox  = FALSE
  )

  # breakfast depends on oatmeal and toast; toast depends on bread
  libpath <- renv_use_libpath()
  expect_true(file.exists(file.path(libpath, "breakfast")))
  expect_true(file.exists(file.path(libpath, "oatmeal")))
  expect_true(file.exists(file.path(libpath, "toast")))
  expect_true(file.exists(file.path(libpath, "bread")))

})

test_that("use(repos = NULL) installs dependencies of already-installed packages", {

  skip_on_cran()

  renv_tests_scope("breakfast")
  init()

  # reset the shared use libpath so we get a fresh library
  renv_scope_binding(the, "use_libpath", NULL)
  renv_scope_libpaths()

  # pre-install breakfast (without dependencies) into the use library,
  # so it will be considered "compatible" during the compat check
  libpath <- renv_use_libpath()
  ensure_directory(libpath)
  renv_use_cacheonly_install(
    records = list(breakfast = list(Package = "breakfast", Source = "Repository")),
    library = libpath
  )
  expect_true(file.exists(file.path(libpath, "breakfast")))
  expect_false(file.exists(file.path(libpath, "oatmeal")))

  use(
    "breakfast",
    repos    = NULL,
    isolate  = TRUE,
    verbose  = FALSE,
    sandbox  = FALSE
  )

  # dependencies should still be resolved and installed
  expect_true(file.exists(file.path(libpath, "oatmeal")))
  expect_true(file.exists(file.path(libpath, "toast")))
  expect_true(file.exists(file.path(libpath, "bread")))

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

# https://github.com/rstudio/renv/issues/1951
test_that("use() warns when a loaded package can't be replaced", {

  skip_on_cran()

  renv_tests_scope()
  renv_scope_libpaths()
  renv_scope_options(renv.caution.verbose = TRUE)

  # load an older version of 'bread'. the name is passed in a variable, as
  # 'R CMD check' takes a literal for an undeclared dependency of the tests
  old <- renv_scope_tempfile("renv-library-")
  ensure_directory(old)
  quietly(install("bread@0.1.0", library = old))
  package <- "bread"
  loadNamespace(package, lib.loc = old)
  defer(unloadNamespace("bread"))
  expect_equal(renv_namespace_version("bread"), "0.1.0")

  stale <- "- bread [0.1.0 loaded, 1.0.0 requested]"
  renv_scope_binding(the, "use_libpath", NULL)

  # 'toast' depends on 'bread', which is installed alongside it
  expect_warning(
    use("toast", isolate = TRUE, sandbox = FALSE, verbose = FALSE),
    stale,
    fixed = TRUE
  )

  # the requested version is installed by now, so nothing gets installed
  expect_warning(
    use("bread@1.0.0", isolate = TRUE, sandbox = FALSE, verbose = FALSE),
    stale,
    fixed = TRUE
  )

  # installing from the cache
  the$use_libpath <- NULL
  expect_warning(
    use("bread@1.0.0", repos = NULL, isolate = TRUE, sandbox = FALSE, verbose = FALSE),
    stale,
    fixed = TRUE
  )

  # installing from a repository; a restart would discard the library
  the$use_libpath <- NULL
  output <- capture.output(
    warnings <- capture_warnings(
      use("bread@1.0.0", isolate = TRUE, sandbox = FALSE, verbose = TRUE)
    )
  )
  expect_length(warnings, 1L)
  expect_match(warnings, stale, fixed = TRUE)
  expect_match(paste(output, collapse = "\n"), "Successfully installed", fixed = TRUE)
  expect_no_match(paste(output, collapse = "\n"), "Restart your R session", fixed = TRUE)

  # install() has no such constraint, so it still suggests a restart
  library <- renv_scope_tempfile("renv-library-")
  ensure_directory(library)
  output <- capture.output(
    install("bread@1.0.0", library = library)
  )
  expect_match(paste(output, collapse = "\n"), "Restart your R session", fixed = TRUE)

})

test_that("use(lockfile) warns when a loaded package can't be replaced", {

  skip_on_cran()
  skip_on_windows()

  renv_tests_scope("bread")
  init()

  old <- renv_scope_tempfile("renv-library-")
  ensure_directory(old)
  quietly(install("bread@0.1.0", library = old))
  package <- "bread"
  loadNamespace(package, lib.loc = old)
  defer(unloadNamespace("bread"))

  stale <- "- bread [0.1.0 loaded, 1.0.0 requested]"
  renv_scope_libpaths()
  renv_scope_binding(the, "use_libpath", NULL)

  expect_warning(
    use(lockfile = "renv.lock", isolate = TRUE, sandbox = FALSE, verbose = FALSE),
    stale,
    fixed = TRUE
  )

  the$use_libpath <- NULL
  expect_warning(
    use(lockfile = "renv.lock", repos = NULL, isolate = TRUE, sandbox = FALSE, verbose = FALSE),
    stale,
    fixed = TRUE
  )

})

test_that("use() doesn't warn about packages the caller can't act on", {

  # renv itself is always loaded, and lockfiles commonly record another version
  records <- list(renv = list(Package = "renv", Version = "0.0.1"))
  expect_no_warning(renv_use_check_loaded(records))

  # records without a version match whatever is loaded
  records <- list(utils = list(Package = "utils"))
  expect_no_warning(renv_use_check_loaded(records))

})
