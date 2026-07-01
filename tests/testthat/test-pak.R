
test_that("renv::install() works in projects using pak", {

  skip_on_cran()  
  skip_on_windows()
  skip_if_not_installed("pak")
  pak <- renv_namespace_load("pak")
  renv_scope_options(renv.config.pak.enabled = TRUE)
  project <- renv_tests_scope()
  
  # try installing an older version of 'breakfast'
  quietly(install("breakfast@0.1.0"))
  expect_true(renv_package_installed("breakfast"))
  expect_equal(renv_package_version("breakfast"), "0.1.0")
  
  # try using 'install()' to update all installed packages
  quietly(install())
  expect_true(renv_package_installed("breakfast"))
  expect_equal(renv_package_version("breakfast"), "1.0.0")
  
})

test_that("install(include = ...) installs requested packages with pak (#2281)", {

  skip_on_cran()
  skip_on_windows()
  skip_if_not_installed("pak")
  pak <- renv_namespace_load("pak")
  renv_scope_options(renv.config.pak.enabled = TRUE)
  project <- renv_tests_scope()

  # install(include = ...) with no positional packages should install the
  # named packages rather than fall through to renv_pak_update().
  quietly(install(include = "breakfast"))
  expect_true(renv_package_installed("breakfast"))

})

test_that("install(include = ...) filters positional packages with pak (#2281)", {

  skip_on_cran()
  skip_on_windows()
  skip_if_not_installed("pak")
  pak <- renv_namespace_load("pak")
  renv_scope_options(renv.config.pak.enabled = TRUE)
  project <- renv_tests_scope()

  # 'bread' and 'oatmeal' are leaf packages with no shared dependencies;
  # include should filter the positional list down to just 'bread', matching
  # the behavior of the non-pak install path
  quietly(install(c("bread", "oatmeal"), include = "bread"))
  expect_true(renv_package_installed("bread"))
  expect_false(renv_package_installed("oatmeal"))

})

test_that("install(exclude = ...) filters positional packages with pak (#2281)", {

  skip_on_cran()
  skip_on_windows()
  skip_if_not_installed("pak")
  pak <- renv_namespace_load("pak")
  renv_scope_options(renv.config.pak.enabled = TRUE)
  project <- renv_tests_scope()

  # exclude should drop the named packages from the positional list,
  # matching the behavior of the non-pak install path
  quietly(install(c("bread", "oatmeal"), exclude = "oatmeal"))
  expect_true(renv_package_installed("bread"))
  expect_false(renv_package_installed("oatmeal"))

})

test_that("install() with an explicit scope filtered to empty is a no-op with pak (#2281)", {

  skip_on_cran()
  skip_on_windows()
  skip_if_not_installed("pak")
  pak <- renv_namespace_load("pak")
  renv_scope_options(
    renv.config.pak.enabled = TRUE,
    renv.verbose = TRUE
  )
  project <- renv_tests_scope()

  # when the caller provides an explicit scope and filtering empties it,
  # we should report "no packages" and return — not fall through to the
  # project-wide pak update path
  output <- capture.output(install("bread", exclude = "bread"))
  expect_match(paste(output, collapse = "\n"), "no packages to install", fixed = TRUE)
  expect_false(renv_package_installed("bread"))

})

test_that("install() forces reinstall of explicit requests, not deps (#2329)", {

  skip_on_cran()
  skip_on_windows()
  skip_if_not_installed("pak")
  pak <- renv_namespace_load("pak")
  renv_scope_options(renv.config.pak.enabled = TRUE)
  renv_tests_scope("bread")

  args <- NULL
  local_mocked_bindings(renv_pak_init = function(...) invisible(NULL))
  local_mocked_bindings(
    .package = "pak",
    pkg_install = function(pkg, upgrade, ...) {
      args <<- list(pkg = pkg, upgrade = upgrade)
      invisible(data.frame(package = character()))
    }
  )

  quietly(install("bread"))

  # the explicitly-requested package is forced to (re)install, so it is
  # installed even when already current; upgrade = FALSE leaves pak's
  # transitively-resolved dependencies (e.g. recommended packages) alone
  expect_equal(unname(args$pkg), "bread?reinstall")
  expect_false(args$upgrade)

})

test_that("install() appends reinstall to specs that already carry a query", {

  skip_on_cran()
  skip_on_windows()
  skip_if_not_installed("pak")
  pak <- renv_namespace_load("pak")
  renv_scope_options(renv.config.pak.enabled = TRUE)
  renv_tests_scope("bread")

  args <- NULL
  local_mocked_bindings(renv_pak_init = function(...) invisible(NULL))
  local_mocked_bindings(
    .package = "pak",
    pkg_install = function(pkg, ...) {
      args <<- pkg
      invisible(data.frame(package = character()))
    }
  )

  # a spec that already has a '?query' must extend it with '&', not a second
  # '?', so pkgdepends parses both parameters
  quietly(install("bread?nocache"))
  expect_equal(unname(args), "bread?nocache&reinstall")

})

test_that("install() does not upgrade transitively-pulled recommended packages (#2329)", {

  skip_on_cran()
  skip_on_windows()
  skip_if_not_installed("pak")
  pak <- renv_namespace_load("pak")
  renv_scope_options(renv.config.pak.enabled = TRUE)
  project <- renv_tests_scope()

  # 'toast' depends on 'bread'; installing 'toast' must not upgrade an
  # already-installed, dependency-satisfying 'bread'.
  quietly(install("bread@0.1.0"))
  expect_equal(renv_package_version("bread"), "0.1.0")

  quietly(install("toast"))
  expect_true(renv_package_installed("toast"))
  expect_equal(renv_package_version("bread"), "0.1.0")

})

test_that("renv::update() works in projects using pak", {

  skip_on_cran()
  skip_on_windows()
  skip_if_not_installed("pak")
  pak <- renv_namespace_load("pak")
  renv_scope_options(renv.config.pak.enabled = TRUE)
  project <- renv_tests_scope()

  # try installing an older version of 'breakfast'
  quietly(install("breakfast@0.1.0"))
  expect_true(renv_package_installed("breakfast"))
  expect_equal(renv_package_version("breakfast"), "0.1.0")

  # try using 'update()' to update all installed packages
  quietly(update())
  expect_true(renv_package_installed("breakfast"))
  expect_equal(renv_package_version("breakfast"), "1.0.0")

})

# https://github.com/rstudio/renv/issues/2282
test_that("renv::init() routes through pak when pak.enabled = TRUE", {

  renv_tests_scope("bread")
  renv_scope_options(renv.config.pak.enabled = TRUE)

  pak_install_packages <- NULL
  local_mocked_bindings(
    renv_pak_init = function(...) invisible(NULL),
    renv_pak_install = function(packages, ...) {
      pak_install_packages <<- packages
      invisible(NULL)
    }
  )

  quietly(init())

  expect_true("bread" %in% pak_install_packages)

})

test_that("hydrate() preserves its return contract when pak handles install", {

  renv_tests_scope("bread")
  renv_scope_options(renv.config.pak.enabled = TRUE)

  local_mocked_bindings(
    renv_pak_init = function(...) invisible(NULL),
    # simulate a pak install: return a data.frame, as pak::pkg_install() does
    renv_pak_install = function(...) data.frame(package = "bread")
  )

  result <- quietly(hydrate(prompt = FALSE, report = FALSE))

  # 'missing' should be NULL on success, not the raw pak install result
  expect_null(result$missing)

})

test_that("hydrate() propagates pak install errors", {

  renv_tests_scope("bread")
  renv_scope_options(renv.config.pak.enabled = TRUE)

  local_mocked_bindings(
    renv_pak_init = function(...) invisible(NULL),
    renv_pak_install = function(...) stop("pak failed")
  )

  expect_error(quietly(hydrate(prompt = FALSE, report = FALSE)), "pak failed")

})

test_that("restore(clean = TRUE) removes unused packages with pak enabled", {

  skip_on_cran()
  skip_on_windows()
  skip_if_not_installed("pak")
  renv_scope_options(renv.config.pak.enabled = TRUE)
  renv_tests_scope("oatmeal")
  init()

  renv_scope_options(renv.config.auto.snapshot = FALSE)
  install("bread")
  expect_true(renv_package_installed("bread"))

  quietly(restore(clean = TRUE))
  expect_false(renv_package_installed("bread"))
  expect_true(renv_package_installed("oatmeal"))

})

test_that("restore(clean = TRUE) aborts when prompt is declined on pak path", {

  skip_on_cran()
  skip_on_windows()
  skip_if_not_installed("pak")
  renv_scope_options(renv.config.pak.enabled = TRUE)
  renv_tests_scope("oatmeal")
  init()

  renv_scope_options(renv.config.auto.snapshot = FALSE)
  install("bread")
  expect_true(renv_package_installed("bread"))

  local_mocked_bindings(proceed = function(...) FALSE)
  expect_error(quietly(restore(clean = TRUE, prompt = TRUE)))
  expect_true(renv_package_installed("bread"))
  expect_true(renv_package_installed("oatmeal"))

})

test_that("restore(clean = TRUE) honors `exclude` on pak path", {

  skip_on_cran()
  skip_on_windows()
  skip_if_not_installed("pak")
  renv_scope_options(renv.config.pak.enabled = TRUE)
  renv_tests_scope("oatmeal")
  init()

  renv_scope_options(renv.config.auto.snapshot = FALSE)
  install("bread")
  expect_true(renv_package_installed("bread"))

  quietly(restore(clean = TRUE, exclude = "bread"))
  expect_true(renv_package_installed("bread"))
  expect_true(renv_package_installed("oatmeal"))

})

test_that("restore(clean = TRUE) honors `packages` on pak path", {

  skip_on_cran()
  skip_on_windows()
  skip_if_not_installed("pak")
  renv_scope_options(renv.config.pak.enabled = TRUE)
  renv_tests_scope("oatmeal")
  init()

  renv_scope_options(renv.config.auto.snapshot = FALSE)
  install("bread")
  expect_true(renv_package_installed("bread"))

  # cleanup is scoped to the named packages; bread is unused but not named,
  # so it should survive.
  quietly(restore(clean = TRUE, packages = "oatmeal"))
  expect_true(renv_package_installed("bread"))
  expect_true(renv_package_installed("oatmeal"))

})
