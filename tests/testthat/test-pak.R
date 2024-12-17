
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
