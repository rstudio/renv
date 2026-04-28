
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
