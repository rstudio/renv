
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

})
